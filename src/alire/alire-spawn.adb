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
                 "-gnatwU" &
                 "-j0" &
                 "-p" &
                 --  Supress warnings on unused (may happen in prj_alr.ads)
                 Extra_Args &
                 "-P" &
                 Project_File,
               Understands_Verbose => True);
   end Gprbuild;

end Alire.Spawn;
