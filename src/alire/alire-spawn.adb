with Alire.Settings.Edit;
with Alire_Early_Elaboration;
with Alire.OS_Lib.Subprocess;

with CLIC.User_Input;

package body Alire.Spawn is

   -------------
   -- Command --
   -------------

   procedure Command
     (Cmd                 : String;
      Args                : AAA.Strings.Vector;
      Understands_Verbose : Boolean := False)
   is
      Unused_Output : AAA.Strings.Vector;
   begin
      if Alire_Early_Elaboration.Switch_Q then
         Unused_Output :=
           Alire.OS_Lib.Subprocess.Checked_Spawn_And_Capture
             (Cmd, Args, Understands_Verbose, Err_To_Out => True);
      else
         Alire.OS_Lib.Subprocess.Checked_Spawn
           (Cmd, Args, Understands_Verbose);
      end if;
   end Command;

   ----------------------
   -- Settings_Command --
   ----------------------

   procedure Settings_Command
     (Cmd          : String;
      Replacements : Alire.Formatting.Replacements;
      Exec_Check   : access procedure (Exec : String) := null)
   is
      Args          : AAA.Strings.Vector := AAA.Strings.Split (Cmd, ' ');
      Exec          : constant String := Args.First_Element;
      Replaced_Args : AAA.Strings.Vector;
   begin
      if Exec_Check /= null then
         Exec_Check (Exec);
      end if;
      if Alire.OS_Lib.Subprocess.Locate_In_Path (Exec) = "" then
         Raise_Checked_Error ("'" & Exec & "' not available or not in PATH.");
      end if;
      Args.Delete_First;
      for Element of Args loop
         Replaced_Args.Append
           (Alire.Formatting.Format (Element, Replacements, False));
      end loop;
      Command (Exec, Replaced_Args);
   end Settings_Command;

   --------------
   -- Gprbuild --
   --------------

   procedure Gprbuild (Project_File : String; Extra_Args : AAA.Strings.Vector)
   is
      use AAA.Strings;
   begin
      if Alire.OS_Lib.Subprocess.Locate_In_Path ("gprbuild") = "" then
         Alire.Raise_Checked_Error
           ("Cannot locate " & TTY.Emph ("gprbuild") & ", please check that " &
            "you have a GNAT and GPRbuild installation in your environment.");
      end if;

      Command ("gprbuild",
               Empty_Vector &
                 "-s"  & -- Recompile if compiler switches have changed
                 "-j0" & -- Build in parallel
                 "-p"  & -- Create missing obj, lib and exec dirs
                 "-P"  & Project_File &
                 Extra_Args,
               Understands_Verbose => True);
   end Gprbuild;

   ----------------
   -- Gprinstall --
   ----------------

   procedure Gprinstall
     (Release      : Releases.Release;
      Project_File : Absolute_File;
      Prefix       : Absolute_Path;
      Recursive    : Boolean;
      Quiet        : Boolean;
      Force        : Boolean := Alire.Force;
      Extra_Args   : AAA.Strings.Vector := AAA.Strings.Empty_Vector)
   is
      use AAA.Strings;
   begin
      Spawn.Command
        ("gprinstall",
         AAA.Strings.Empty_Vector
         & (if Recursive then To_Vector ("-r") else Empty_Vector)
         & (if Quiet     then To_Vector ("-q") else Empty_Vector)
         & (if Force     then To_Vector ("-f") else Empty_Vector)
         & String'("--install-name=" & Release.Milestone.Image)
         & "-m" -- minimal install (only needed sources)
         & "-p" -- create missing dirs
         & "--link-lib-subdir=bin"
         --  Softlinks in same dir as executables, saves a path on Windows
         & "--mode=usage" -- omit unwanted devel files
         & String'("--prefix=" & Prefix)
         & "-P" & Project_File
         & Extra_Args
        );
   end Gprinstall;

   -----------------------------
   -- Recreate_Global_Options --
   -----------------------------

   function Recreate_Global_Options return AAA.Strings.Vector is
      use AAA.Strings;
      package AEE renames Alire_Early_Elaboration;
      package UI renames CLIC.User_Input;

      Res : Vector := Empty_Vector;
   begin
      if AEE.Switch_D then
         Res.Append ("-d");
      end if;

      if AEE.Switch_VV then
         Res.Append ("-vv");
      elsif AEE.Switch_V then
         Res.Append ("-v");
      elsif AEE.Switch_Q then
         Res.Append ("-q");
      end if;

      if UI.Not_Interactive then
         Res.Append ("-n");
      end if;

      if Alire.Force then
         Res.Append ("-f");
      end if;

      if not CLIC.TTY.Color_Enabled then
         Res.Append ("--no-color");
      end if;

      if not CLIC.TTY.Is_TTY then
         Res.Append ("--no-tty");
      end if;

      if not Alire.Settings.Edit.Is_At_Default_Dir then
         Res.Append
           (String'("""--settings=" & Alire.Settings.Edit.Path & """"));
      end if;

      return Res;
   end Recreate_Global_Options;

end Alire.Spawn;
