with Ada.Directories;

with Alire.OS_Lib;

with Alr.Project;
with Alr.Rolling;
with Alr.Templates;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with System.Multiprocessors;

package body Alr.Bootstrap is

   Alr_Exec : constant String := Alr_Src_Folder / "obj" / "alr";

   ----------------------------
   -- Respawn_With_Canonical --
   ----------------------------

   procedure Respawn_With_Canonical (Command_Line : String := Current_Command_Line) is
   begin
      if Is_Executable_File (Alr_Exec) then
         Log ("...");
         OS_Exit (Alire.OS_Lib.Spawn (Alr_Exec, Command_Line));
         -- NOTE: THIS IS THE END OF EXECUTION OF THE CALLING alr
      else
         Log ("alr executable not found at " & Alr_Exec & ", not respawning");
      end if;
   end Respawn_With_Canonical;

   ----------------------------------
   -- Check_If_Rolling_And_Respawn --
   ----------------------------------

   procedure Check_If_Rolling_And_Respawn is
   begin
      if Rolling.Enabled then
         Log ("alr up to date (session: " & Session.Hash & ")");
      else
         if Is_Executable_File (Alr_Exec) then
            Respawn_With_Canonical;
         else
            Log ("alr executable may be out of date, consider running ""alr update --alr""");
         end if;
      end if;
   end Check_If_Rolling_And_Respawn;

   -------------------------------------------
   -- Check_If_Project_Outdated_And_Rebuild --
   -------------------------------------------

   procedure Check_If_Project_Outdated_And_Rebuild is
   begin
      if Running_In_Session and then not Session_Is_Current then
         Rebuild (OS_Lib.Locate_Any_Index_File);
      end if;
   end Check_If_Project_Outdated_And_Rebuild;

   ---------------------------
   -- Check_Rebuild_Respawn --
   ---------------------------

   procedure Check_Rebuild_Respawn is
   begin
      if not Running_In_Session then
         Log ("Could not find alr session, stopping now");
         raise Command_Failed;
      end if;

      if not Session_Is_Current then
         Rebuild (OS_Lib.Locate_Any_Index_File);
         Respawn_With_Canonical;
      end if;

      if not Running_In_Project then
         raise Command_Failed;
      end if;
   end Check_Rebuild_Respawn;

   -------------
   -- Rebuild --
   -------------

   procedure Rebuild (Alr_File : String := "") is
      use Ada.Directories;

      Folder_To_Index : constant String :=
                          Alr_Src_Folder / "deps" / "alire" / "index";
   begin
      Log ("Generating index for " & Folder_To_Index);
      Templates.Generate_Index (OS.Session_Folder, Folder_To_Index);

      if Alr_File /= "" then
         Log ("Generating session for " & Alr_File);
         Templates.Generate_Session (OS.Session_Folder, Alr_File);
         Copy_File (Alr_File, OS.Session_Folder / Simple_Name (Alr_File), "mode=overwrite");
      end if;

      Alire.OS_Lib.Spawn
        ("gprbuild",
         "-p -g -m -j" & Utils.Trim (System.Multiprocessors.Number_Of_CPUs'Img) & " " &
           "-XROLLING=True -XSELFBUILD=True " &
           "-XSESSION=" & (if Alr_File /= "" then OS.Session_Folder
                                             else Alr_Src_Folder / "src" / "default_session") & " " &
         "-P" & (Alr_Src_Folder / "alr_env.gpr"));
   end Rebuild;

   ----------------------------------
   -- Rebuild_With_Current_Project --
   ----------------------------------

   procedure Rebuild_With_Current_Project is
   begin
      if Running_In_Session then
         Rebuild (Locate_Any_Index_File);
      else
         Rebuild;
      end if;
   end Rebuild_With_Current_Project;

   ------------------------
   -- Running_In_Project --
   ------------------------

   function Running_In_Project return Boolean is
   begin
      if not Running_In_Session then
         return False;
      end if;

      if Project.Current.Is_Empty then
         Log ("No internal root project, cannot verify external");
         return False;
      end if;

      declare
         Gpr : constant Alire.Project_Name :=
                 Project.Current.Element.Project & "_alr.ads";
      begin
         if Is_Regular_File (Gpr) then
            return True;
         else
            Log ("Project file " & Utils.Quote (Gpr) & " not in current folder");
            return False;
         end if;
      end;
   end Running_In_Project;

end Alr.Bootstrap;
