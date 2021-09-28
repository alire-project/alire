with Alire.Spawn;

package body Alr.Commands.Exec is

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      use AAA.Strings;
   begin
      if Args.Is_Empty then
         Reportaise_Wrong_Arguments
           (Cmd.Name & " takes at least one argument");
      end if;

      declare
         Cmd_Args : AAA.Strings.Vector := Args;
         Cmd_Name : constant String := Cmd_Args.First_Element;
      begin
         Cmd.Requires_Valid_Session;
         Cmd.Root.Export_Build_Environment;

         --  Remove command name from the arguments
         Cmd_Args.Delete_First;

         Alire.Spawn.Command (Cmd                 => Cmd_Name,
                              Args                => Cmd_Args,
                              Understands_Verbose => False);
      end;
   end Execute;

end Alr.Commands.Exec;
