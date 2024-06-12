with Alire.Warnings;

package body Alr.Commands.Config is

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
   begin
      Alire.Warnings.Warn_Once ("`alr " & Command_Name & "` is " &
                                  CLIC.TTY.Error ("deprecated") & ". " &
                                  "Please use `alr " & Settings.Command_Name &
                                  "` instead.");
      Parent (Cmd).Execute (Args);
   end Execute;

end Alr.Commands.Config;
