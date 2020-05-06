with Alr.Root;
with Alr.Build_Env;

package body Alr.Commands.Setenv is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      pragma Unreferenced (Cmd);
   begin
      Requires_Full_Index;

      Requires_Valid_Session;

      --  Temporarily raise the log level to avoid spurious status output that
      --  would prevent directly sourcing the output. TODO: remove this once
      --  the lockfile is used, that will make this unnecessary.
      declare
         Old_Level : constant Simple_Logging.Levels := Alire.Log_Level;
      begin
         Alire.Log_Level := Simple_Logging.Always;
         Alr.Build_Env.Print (Alr.Root.Current);
         Alire.Log_Level := Old_Level;
      end;
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute is
      Cmd : Command;
   begin
      Execute (Cmd);
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("Print the environment variables used to build the crate." &
                 " This command can be used to setup a build enviroment, for" &
                 " instance before starting an IDE"));

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      pragma Unreferenced (Cmd, Config);
   begin
      null;
   end Setup_Switches;

end Alr.Commands.Setenv;
