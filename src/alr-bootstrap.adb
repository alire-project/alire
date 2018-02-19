with Ada.Directories;

with Alr.Devel;
with Alr.Hardcoded;
with Alr.OS;
with Alr.OS_Lib;
with Alr.Project;
with Alr.Session;
with Alr.Spawn;
with Alr.Templates;
with Alr.Utils;

with GNAT.Ctrl_C;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Alr.Bootstrap is

   use OS_Lib.Paths;

   Executable      : constant String := Hardcoded.Alr_Src_Folder / "bin" / "alr";
   Executable_Bak  : constant String := Hardcoded.Alr_Src_Folder / "bin" / "alr-prev";

   ----------------
   -- Is_Rolling --
   ----------------

   function Is_Rolling return Boolean is
     (OS.Own_Executable = Hardcoded.Alr_Exe_File);

   ----------------------------------
   -- Check_If_Rolling_And_Respawn --
   ----------------------------------

   procedure Check_If_Rolling_And_Respawn is
   begin
      if not Is_Rolling then
         if Is_Executable_File (Hardcoded.Alr_Exe_File) then
            Spawn.Updated_Alr_Without_Return;
         else
            Log ("alr executable may be out of date, consider running ""alr update --online""");
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
         Log ("Could not find alr session, stopping now", Warning);
         raise Command_Failed;
      end if;

      if not Session_Is_Current then
         Trace.Debug ("About to rebuild with new session");
         Rebuild (OS_Lib.Locate_Any_Index_File);
         Spawn.Updated_Alr_Without_Return;
      end if;

      if not Running_In_Project then
         raise Command_Failed;
      end if;
   end Check_Rebuild_Respawn;

   -----------------
   -- Interrupted --
   -----------------

   procedure Interrupted is
      use Ada.Directories;
   begin
      Trace.Always (" Interrupted by user");

      --  Attempt to leave the previous alr exec in its place
      if Exists (Executable_Bak) and then
        (not Exists (Executable) or else not Is_Executable_File (Executable))
      then
         Trace.Debug ("Restoring alr from backup");
         Copy_File (Executable_Bak, Executable, "mode=overwrite,preserve=all_attributes");
      end if;
   end Interrupted;

   -------------
   -- Rebuild --
   -------------

   procedure Rebuild (Alr_File : String := "") is
      use Ada.Directories;
      use Hardcoded;

      Folder_To_Index : constant String := Alr_Src_Folder / "deps" / "alire" / "index";
   begin
      --  It seems .ali files aren't enough to detect changed files under a second,
      --  So we get rid of previous ones
      if Exists (Executable) then
         if Exists (Executable_Bak) then
            Delete_File (Executable_Bak);
         end if;
         Rename (Executable, Executable_Bak);
      end if;


      OS_Lib.Delete_File (Alr_Src_Folder / "obj" / "alr-main.bexch");
      OS_Lib.Delete_File (Alr_Src_Folder / "obj" / "alr-main.ali");
      OS_Lib.Delete_File (Alr_Src_Folder / "obj" / "alr-session.ali");

      if Alr_File /= "" then
         OS_Lib.Delete_File (Alr_Src_Folder / "obj" /
                               Utils.Replace (Simple_Name (Alr_File), ".ads", ".ali"));
      end if;

      --  This could be an alternative if we don't want to delete the current exec
      Log ("About to recompile...", Debug);
      --  delay 1.0;

      Log ("Generating index for " & Folder_To_Index, Detail);
      Templates.Generate_Index (OS.Session_Folder, Folder_To_Index);

      if Alr_File /= "" then
         Log ("Generating session for " & Alr_File, Detail);
         Templates.Generate_Session (OS.Session_Folder, Alr_File);
         Copy_File (Alr_File, OS.Session_Folder / Simple_Name (Alr_File), "mode=overwrite");
      end if;

      begin
         Spawn.Gprbuild (Hardcoded.Alr_Gpr_File,
                         (if Alr_File /= ""
                          then OS.Session_Folder
                          else Hardcoded.Alr_Default_Session_Folder));
      exception
         when others =>
            -- Compilation failed
            if Alr_File = "" then
               Log ("alr self-build failed. Since you are not inside an alr project,");
               Log ("the error is likely in alr itself. Please report your issue to the developers.");
            else
               Log ("alr self-build failed. Please verify the syntax in your project dependency file.");
               Log ("The dependency file in use is: " & Alr_File);
            end if;

            raise Command_Failed;
      end;
   end Rebuild;

   ----------------------------------
   -- Rebuild_With_Current_Project --
   ----------------------------------

   procedure Rebuild_With_Current_Project is
   begin
      if Running_In_Session then
         Rebuild (Os_Lib.Locate_Any_Index_File);
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
         Gpr : constant String :=
                 Hardcoded.Project_File (Project.Current.Element.Project);
      begin
         if Is_Regular_File (Gpr) then
            return True;
         else
            Log ("Project file " & Utils.Quote (Gpr) & " not in current folder");
            return False;
         end if;
      end;
   end Running_In_Project;

   ------------------------
   -- Running_In_Session --
   ------------------------

   function Running_In_Session return Boolean is
     (OS_Lib.Locate_Any_GPR_File > 0 and Then
      OS_Lib.Locate_Any_Index_File /= "");

   ------------------------
   -- Session_Is_Current --
   ------------------------

   function Session_Is_Current return Boolean is
     (Session.Hash = Utils.Hash_File (OS_Lib.Locate_Any_Index_File));

   -----------------
   -- Status_Line --
   -----------------

   function Status_Line return String is
   begin
      return
           (if Is_Rolling then "rolling" else "bootstrap") & "-" &
           (if Devel.Enabled then "devel" else "release") &
           " (" &
           (if Running_In_Session
            then (if Session_Is_Current then Project.Current.Element.Milestone_Image else "outdated")
            else "no project") & ") (" &
            Utils.Trim (Alire.Index.Releases.Length'Img) & " releases indexed)";
   end Status_Line;

begin
   GNAT.Ctrl_C.Install_Handler (Interrupted'Access);
end Alr.Bootstrap;
