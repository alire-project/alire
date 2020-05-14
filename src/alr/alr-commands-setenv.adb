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

      Alr.Build_Env.Print (Alr.Root.Current);
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
                 " This command can be used to setup a build environment," &
                 " for instance before starting an IDE"));

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
