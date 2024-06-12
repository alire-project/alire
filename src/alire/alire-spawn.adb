with Alire_Early_Elaboration;
with Alire.OS_Lib.Subprocess;

package body Alire.Spawn is

   -------------
   -- Command --
   -------------

   procedure Command (Cmd                 : String;
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

   --------------
   -- Gprbuild --
   --------------

   procedure Gprbuild (Project_File : String;
                       Extra_Args   : AAA.Strings.Vector)
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

end Alire.Spawn;
