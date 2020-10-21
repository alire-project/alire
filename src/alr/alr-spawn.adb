with Alire.OS_Lib.Subprocess;
with Alire.Paths;
with Alire.Utils.TTY;

with Alr.Commands;

package body Alr.Spawn is

   package TTY renames Alire.Utils.TTY;

   -------------
   -- Command --
   -------------

   procedure Command (Cmd                 : String;
                      Args                : Alire.Utils.String_Vector;
                      Understands_Verbose : Boolean := False)
   is
      Unused_Output : Alire.Utils.String_Vector;
   begin
      if Commands.Is_Quiet then
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
                       Extra_Args   : Alire.Utils.String_Vector)
   is
      use Alire.Utils;

      Relocate : constant String :=
        "--relocate-build-tree=" & Alire.Paths.Build_Folder;
   begin
      if Alire.OS_Lib.Subprocess.Locate_In_Path ("gprbuild") = "" then
         Alire.Raise_Checked_Error
           ("Cannot locate " & TTY.Emph ("gprbuild") & ", please check that " &
            "you have a GNAT and GPRbuild installation in your environment.");
      end if;

      Command ("gprbuild",
               Empty_Vector &
                 "-gnatwU" &
                 "-j0" &
                 "-p" &
                 --  Supress warnings on unused (may happen in prj_alr.ads)
                 Extra_Args &
                 "-P" &
                 Project_File &
                 "--root-dir=." &
                 Relocate,
               Understands_Verbose => True);
   end Gprbuild;

end Alr.Spawn;
