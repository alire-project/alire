with Ada.Calendar;
with Ada.Directories;

with Alire_Early_Elaboration;

with Alr.Commands.Update;
with Alr.Files;
with Alr.Hardcoded;
with Alr.OS_Lib;
with Alr.Project;
with Alr.Platform;
with Alr.Self;
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

   -----------------------------
   -- Attempt_Backup_Recovery --
   -----------------------------

   procedure Attempt_Backup_Recovery is
      use Ada.Directories;
   begin
      --  Attempt to leave the previous alr exec in its place
      if Exists (Executable_Bak) and then
        (not Exists (Executable) or else not Is_Executable_File (Executable))
      then
         Trace.Debug ("Restoring alr from backup");
         Copy_File (Executable_Bak, Executable, "mode=overwrite,preserve=all_attributes");
      end if;
   end Attempt_Backup_Recovery;

   ---------------------
   -- Check_Ada_Tools --
   ---------------------

   procedure Check_Ada_Tools is
      --  FIXME mini-leak (once per run)
   begin
      if Locate_Exec_On_Path ("gprbuild") = null or else
        Locate_Exec_On_Path ("gnatmake") = null then
         Trace.Error ("Ada compiler not detected, alr cannot proceed");
         OS_Lib.Bailout (1);
      end if;

      if Locate_Exec_On_Path ("git") = null then
         Trace.Warning ("git is not detected, alr will fail on most operations");
      end if;

      if Locate_Exec_On_Path ("hg") = null then
         Trace.Warning ("hg is not detected, alr will fail on mercurial checkouts");
      end if;
   end Check_Ada_Tools;

   ----------------------------------
   -- Check_If_Rolling_And_Respawn --
   ----------------------------------

   procedure Check_If_Rolling_And_Respawn is
   begin
      if not Self.Is_Rolling then
         if Is_Executable_File (Hardcoded.Alr_Rolling_Exe_File) then
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
         Rebuild (Files.Locate_Any_Index_File);
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
         Rebuild (Files.Locate_Any_Index_File);
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

      Attempt_Backup_Recovery;
      OS_Lib.Bailout (1);
   end Interrupted;

   -------------
   -- Rebuild --
   -------------

   procedure Rebuild (Alr_File : String := "") is
      use Ada.Directories;
      use Hardcoded;

      Folder_To_Index : constant String := Alr_Src_Folder / "deps" / "alire" / "index";
   begin
      --  Before rebuilding we need the sources to exist!
      --  Note that the first time we run after developer build, alr considers itself a release build
      --  and won't change into a devel build until self-compiled once.
      if not Exists (Alr_Src_Folder) then
         Trace.Detail ("Checking out alr sources...");
         Commands.Update.Update_Alr;
      end if;

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
         OS_Lib.Delete_File (Alr_Src_Folder / "obj" / Utils.Replace (Simple_Name (Alr_File), ".ads", ".ali"));
      end if;

      --  This could be an alternative if we don't want to delete the current exec
      Log ("About to recompile...", Debug);
      --  delay 1.0;

      --  CONFIG FILE
      --  We copy it anyway. If building without SELFBUILD (e.g. developer build)
      --  the default one will be used but no conflict can happen anyway
--        declare
--           Config_Found : constant Boolean :=
--                            Exists (OS.Config_Folder / Hardcoded.Alr_Conf_File);
--           Origin : constant String :=
--                      (if Config_Found
--                       then OS.Config_Folder                         / Hardcoded.Alr_Conf_File
--                       else Hardcoded.Alr_Src_Default_Session_Folder / Hardcoded.Alr_Conf_File);
--        begin
--           Trace.Detail ((if Config_Found
--                         then "Found config file"
--                         else "Config file not found") & ", copying " &
--                           Origin &
--                           " -> " &
--                           Hardcoded.Session_Folder / Hardcoded.Alr_Conf_File);
--           Copy_File (Origin,
--                      Hardcoded.Session_Folder / Hardcoded.Alr_Conf_File,
--                      "mode=overwrite");
--        end;

      --  INDEX FILE
      Log ("Generating index for " & Folder_To_Index, Detail);
      Templates.Generate_Full_Index (Hardcoded.Session_Folder, Folder_To_Index);

      --  METADATA FILE
      if Alr_File /= "" then
         Copy_File (Alr_File, Hardcoded.Session_Folder / Simple_Name (Alr_File), "mode=overwrite");

         Log ("Generating session for " & Alr_File, Detail);
      else
         Log ("Generating non-project session", Detail);
      end if;

      --  SESSION FILE
      Templates.Generate_Session (Hardcoded.Session_Folder, Alr_File);

      begin
         Spawn.Gprbuild (Hardcoded.Alr_Gpr_File, Hardcoded.Session_Folder);
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
            Trace.Info ("");
            Trace.Info ("Re-run with -v or -d for details");

            Attempt_Backup_Recovery;

            raise Command_Failed;
      end;
   end Rebuild;

   ----------------------------------
   -- Rebuild_With_Current_Project --
   ----------------------------------

   procedure Rebuild_With_Current_Project is
   begin
      if Running_In_Session then
         Rebuild (Files.Locate_Any_Index_File);
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
         Trace.Debug ("No session, rebuild needed before being in project");
         return False;
      end if;

      if Project.Is_Empty then
         Trace.Debug ("No internal root project, cannot verify external");
         return False;
      end if;

      --  Is this check really necessary?
      declare
         Gprs : constant Utils.String_Vector := Project.Current.GPR_Files (Platform.Properties);
      begin
         for Gpr of Gprs loop
            if not Is_Regular_File (Gpr) then
               Trace.Warning ("Project file " & Utils.Quote (Gpr) & " not found");
               return False;
            end if;
         end loop;
      end;

      return True;
   end Running_In_Project;

   ------------------------
   -- Running_In_Session --
   ------------------------

   function Running_In_Session return Boolean is
     (Files.Locate_Any_GPR_File > 0 and Then
      Files.Locate_Any_Index_File /= "");

   ------------------------
   -- Session_Is_Current --
   ------------------------

   function Session_Is_Current return Boolean is
     (Session.Hash = Utils.Hash_File (Files.Locate_Any_Index_File));

   -----------------
   -- Status_Line --
   -----------------

   function Status_Line return String is
      use Ada.Calendar;
      type Milliseconds is delta 0.001 range 0.0 .. 24.0 * 60.0 * 60.0;
   begin
      return
        (if Self.Is_Rolling then "rolling" else "bootstrap") & "-" &
        (if not Self.Is_Canonical then "devel" else "release") &
        " (" &
        (if Running_In_Session
         then (if Session_Is_Current then Project.Current.Milestone.Image else "outdated")
         else "no project") & ") (" &
        Utils.Trim (Alire.Index.Catalog.Length'Img) & " releases indexed)" &
        (if Self.Is_Bootstrap then " (minimal index)" else "") &
        ("(loaded in" & Milliseconds'Image (Milliseconds (Ada.Calendar.Clock - Alire_Early_Elaboration.Start)) & "s)");
   end Status_Line;

begin
   GNAT.Ctrl_C.Install_Handler (Interrupted'Access);
end Alr.Bootstrap;
