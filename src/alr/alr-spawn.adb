with Alire.Paths;

with Alire.OS_Lib.Subprocess;

package body Alr.Spawn is

   -------------
   -- Command --
   -------------

   procedure Command (Cmd                 : String;
                      Args                : Alire.Utils.String_Vector;
                      Understands_Verbose : Boolean := False)
   is
   begin
      Alire.OS_Lib.Subprocess.Checked_Spawn (Cmd, Args, Understands_Verbose);
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
