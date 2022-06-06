with Alire_Early_Elaboration;
with Ada.Directories;
with Alire.Config;
with Alire.Paths;
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
      use Alire.OS_Lib.Operators;

      Containing_Directory : constant String :=
        Ada.Directories.Containing_Directory (Project_File);

      Toolchain : constant String :=
        Alire.Config.DB.Get
          (Alire.Config.Keys.Toolchain_Use & ".gnat",
           Alire.Paths.Cache_Project_File_Name);

      Autoconf_File : constant String :=
        Containing_Directory /
          Alire.Paths.Working_Folder_Inside_Root /
            Alire.Paths.Cache_Folder_Inside_Working_Folder /
              Toolchain;

      Autoconf_Param : constant String :=
        "--autoconf=" & Autoconf_File;

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
                 Autoconf_Param &
                 Extra_Args,
               Understands_Verbose => True);
   end Gprbuild;

end Alire.Spawn;
