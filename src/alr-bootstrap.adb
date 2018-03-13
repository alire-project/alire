with Ada.Calendar;
with Ada.Directories;

with Alire_Early_Elaboration;

with Alr.Commands.Update;
with Alr.Files;
with Alr.Hardcoded;
with Alr.OS_Lib;
with Alr.Root;
with Alr.Self;
with Alr.Spawn;
with Alr.Templates;
with Alr.Utils;

with GNAT.Ctrl_C;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Alr.Bootstrap is

   use OS_Lib.Paths;

   Executable      : constant String := Hardcoded.Alr_Src_Folder / "bin" / "alr";
   Executable_Bak  : constant String := Hardcoded.Alr_Src_Folder / "bin" / "alr-prev";

   Cached_Metafile : access String;

   function Metadata_File return String is
   begin
      if Cached_Metafile = null then
         Cached_Metafile := new String'(Files.Locate_Metadata_File);
         Trace.Detail ("Metadata file in use is: " & Cached_Metafile.all);
      end if;

      return Cached_Metafile.all;
   end Metadata_File;

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
      if OS_Lib.Getenv (Hardcoded.Alr_Child_Flag) /= "" then
         Is_Child := True;
         Trace.Debug ("alr spawned as child");
      end if;

--        Trace.Debug ("SESSION: " & Self.Is_Session'Img);
--        Trace.Debug ("ROLLING: " & Self.Is_Rolling'Img);
--        Trace.Debug ("EMPTY  : " & Root.Is_Empty'Img);

      --  Sanity checks (this happened prior to volatile session variables)
      if not Self.Is_Session and then not Root.Is_Empty then
         raise Program_Error with "Session set and root set shouldn't happen";
      elsif Self.Is_Session and then Root.Is_Empty then
         raise Program_Error with "Session set and root unset shouldn't happen";
      end if;

      if Self.Is_Session then
         -- OK, running specific build for current project
         Trace.Debug ("Initial respawn check: session build, proceeding");
      elsif Self.Is_Rolling then
         null;
      else -- not session and not rolling
         Trace.Debug ("Initial respawn check: launcher build (not rolling)");
         if Is_Executable_File (Hardcoded.Alr_Rolling_Exec) then
            Spawn.Updated_Alr_Without_Return;
         else
            Trace.Warning ("alr executable may be out of date, consider running ""alr update --online""");
         end if;
      end if;
   end Check_If_Rolling_And_Respawn;

   ---------------------------
   -- Check_Rebuild_Respawn --
   ---------------------------

   procedure Check_Rebuild_Respawn is
      Metafile     : constant String  := Files.Locate_Metadata_File;
      Must_Rebuild :          Boolean := False;
   begin
      case Session_State is
         when Detached =>
            Trace.Debug ("Running main alr seeing session file " & Metafile);
            --  Proceed
         when Valid    =>
            Trace.Debug ("Already running session executable");
            return;
         when others   =>
            Trace.Error ("Cannot find session, state is: " & Session_State'Img);
            raise Command_Failed;
      end case;

      if not OS_Lib.Is_Executable_File (Hardcoded.Alr_Session_Exec (Metafile)) then
         Trace.Debug ("Building first session-specific alr");
         Must_Rebuild := True;
      elsif Os_Lib.Is_Older (This => Hardcoded.Alr_Session_Exec (Metafile), Than => Metafile) then
         Trace.Debug ("Rebuilding currently outdated session-specific alr");
         Must_Rebuild := True;
      elsif Os_Lib.Is_Older (This => Hardcoded.Alr_Session_Exec (Metafile), Than => Hardcoded.Alr_Rolling_Exec) then
         Trace.Debug ("Rebuilding older session-specific alr than rolling alr");
         Must_Rebuild := True;
      end if;

      if Must_Rebuild then
         --  We must rebuild and respawn
         Trace.Debug ("About to rebuild with metadata: " & Metafile);
         Rebuild_Respawn (Metafile);
      else
         --  No reason to rebuild, hence we must spawn from rolling alr to session alr:
         Spawn.Session_Alr_Without_Return (Metafile);

--           Trace.Error ("No reason to rebuild, yet session status is not valid!");
--           Trace.Debug ("Target exec  : " & Hardcoded.Alr_Session_Exec (Metafile));
--           Trace.Debug ("Metadata file: " & Metafile);
--           Trace.Debug ("Meta_Is_Newer: " & Boolean'(not Os_Lib.Is_Older (This => Hardcoded.Alr_Session_Exec (Metafile),
--                                                                          Than => Metafile))'Img);
--           Trace.Debug ("Roll_Is_Newer: " & Boolean'(not Os_Lib.Is_Older (This => Hardcoded.Alr_Session_Exec (Metafile),
--                                                                          Than => Hardcoded.Alr_Rolling_Exec))'Img);
--           raise Program_Error;
      end if;

      --  Once here, everything must be all-right
      if Session_State /= Valid then
         raise Command_Failed;
      end if;
   end Check_Rebuild_Respawn;

   -----------------
   -- Interrupted --
   -----------------

   procedure Interrupted is
      use Ada.Directories;
   begin
      if not Is_Child then
         Trace.Always (" Interrupted by user");
      end if;

      Attempt_Backup_Recovery;
      OS_Lib.Bailout (1);
   end Interrupted;

   -------------
   -- Rebuild --
   -------------

   procedure Rebuild (Alr_File : String := "") is
      use Ada.Directories;
      use Hardcoded;

      Folder_To_Index : constant String := Hardcoded.Alr_Index_Folder_Absolute;
      Full_Index      : constant Boolean := Alr_File = "";
      Session_Folder  : constant String := (if Alr_File /= ""
                                            then Hardcoded.Session_Folder (Alr_File)
                                            else Hardcoded.No_Session_Folder);
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
      if Alr_File /= "" then
         Os_Lib.Delete_File (Hardcoded.Alr_Session_Exec (Alr_File));
      else -- rolling exec: we must try to preserve at least a fallback copy
         if Exists (Executable) then
            OS_Lib.Delete_File (Executable_Bak);
            Rename (Executable, Executable_Bak);
         end if;
      end if;

      --  Empirically determined: below second consecutive compilations will generate
      --    corrupted binaries unless these are forced to be recreated:
      OS_Lib.Delete_File (Alr_Src_Folder / "obj" / "alr-main.bexch");
      OS_Lib.Delete_File (Alr_Src_Folder / "obj" / "alr-main.ali");
      OS_Lib.Delete_File (Alr_Src_Folder / "obj" / "alr-main.o");
      OS_Lib.Delete_File (Alr_Src_Folder / "obj" / "b__alr-main.ali");
      OS_Lib.Delete_File (Alr_Src_Folder / "obj" / "b__alr-main.o");
      OS_Lib.Delete_File (Alr_Src_Folder / "obj" / "alr-session-self.ali");
      OS_Lib.Delete_File (Alr_Src_Folder / "obj" / "alr-session-self.o");
      OS_Lib.Delete_File (Alr_Src_Folder / "obj" / "alr-session.ali");
      OS_Lib.Delete_File (Alr_Src_Folder / "obj" / "alr-session.o");

      if Alr_File /= "" then
         OS_Lib.Delete_File (Alr_Src_Folder / "obj" / Utils.Replace (Simple_Name (Alr_File), ".ads", ".ali"));
         OS_Lib.Delete_File (Alr_Src_Folder / "obj" / Utils.Replace (Simple_Name (Alr_File), ".ads", ".o"));
      end if;

      --  DELETE SESSION FOLDER TO ENSURE FRESH FILES
      --  They are going to be generated, so any leftover is unintended!
      if Ada.Directories.Exists (Session_Folder) then
         Ada.Directories.Delete_Tree (Session_Folder);
      end if;
      OS_Lib.Create_Folder (Session_Folder);

      Log ("About to recompile...", Debug);
      --  This could be an alternative if we don't want to delete the current exec
      --  delay 1.0;

      --  INDEX FILE
      if Full_Index then
         Trace.Detail ("Generating index for " & Folder_To_Index);
      else
         Trace.Detail ("Using minimal index");
      end if;

      --  METADATA FILE
      if Alr_File /= "" then
         Copy_File (Alr_File, Session_Folder / Simple_Name (Alr_File), "mode=overwrite");
         Log ("Generating session for " & Alr_File, Detail);
      else
         Log ("Generating non-project session", Detail);
      end if;

      --  SESSION FILE
      Templates.Generate_Session (Session_Folder, Full_Index, Alr_File);

      begin
         Spawn.Gprbuild (Hardcoded.Alr_Gpr_File,
                         Session_Build => Alr_File /= "",
                         Session_Path  => Session_Folder);
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

   ---------------------
   -- Rebuild_Respawn --
   ---------------------

   procedure Rebuild_Respawn (Metafile : String := "") is
   begin
      Rebuild (Metafile);
      if Metafile /= "" then
         Spawn.Session_Alr_Without_Return (Metafile);
      else
         Spawn.Updated_Alr_Without_Return;
      end if;
   end Rebuild_Respawn;

   -------------------
   -- Session_State --
   -------------------

   function Session_State return Session_States is
   begin
      if Self.Is_Session then
         if Self.Matches_Session (Files.Locate_Metadata_File) then
            return Valid;
         else
            return Erroneous;
         end if;
      elsif Files.Locate_Any_GPR_File > 0 and then Files.Locate_Metadata_File /= "" then
         return Detached;
      else
         return Outside;
      end if;
   end Session_State;

   -----------------
   -- Status_Line --
   -----------------

   function Status_Line return String is
      use Ada.Calendar;
      type Milliseconds is delta 0.001 range 0.0 .. 24.0 * 60.0 * 60.0;
   begin
      return
        (if Self.Is_Session then "session" else
           (if Self.Is_Rolling then "rolling" else "launcher")) & "-" &
        (if not Self.Is_Canonical then "devel" else "release") &
        " (" &
        (case Session_State is
            when Erroneous => "erroneous session state",
            when Outside   => "no project",
            when Detached  => "in project",
            when Valid     => "session up to date") &
         ") (" &
        Utils.Trim (Alire.Index.Catalog.Length'Img) & " releases indexed)" &
        (if Self.Is_Bootstrap then " (bootstrap)" else "") &
        (if Self.Has_Full_Index then " (full index)" else " (minimal index)") &
        (" (loaded in" & Milliseconds'Image (Milliseconds (Ada.Calendar.Clock - Alire_Early_Elaboration.Start)) & "s)");
   end Status_Line;

begin
   GNAT.Ctrl_C.Install_Handler (Interrupted'Access);
end Alr.Bootstrap;
