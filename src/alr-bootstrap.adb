with Ada.Directories;

with Alire.OS_Lib;

with Alr.Devel;
with Alr.Rolling;
with Alr.Templates;

with GNAT.OS_Lib; use GNAT.OS_Lib;

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

   -------------
   -- Rebuild --
   -------------

   procedure Rebuild (Project_File : String := "") is
      Source_Folder   : constant String :=
                          (if Devel.Enabled and then Is_Regular_File ("alr_env.gpr")
                           then Ada.Directories.Current_Directory
                           else Alr_Src_Folder);
      Folder_To_Index : constant String :=
                          Source_Folder / "deps" / "alire" / "index";
   begin
      Log ("Generating index for " & Folder_To_Index);
      Templates.Generate_Index (OS.Session_Folder, Folder_To_Index);

      if Project_File /= "" then
         Log ("Generating session for " & Project_File);
         Templates.Generate_Session (OS.Session_Folder, Project_File);
      end if;

      Alire.OS_Lib.Spawn
        ("gprbuild",
         "-p -XROLLING=True -XSELFBUILD=True " &
           "-XSESSION=" & (if Project_File /= "" then OS.Session_Folder
                                                 else Source_Folder / "src" / "default_session") & " " &
         "-P" & (Source_Folder / "alr_env.gpr"));
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

end Alr.Bootstrap;
