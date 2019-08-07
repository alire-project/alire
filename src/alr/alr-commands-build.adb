with Alr.Commands.Compile;
with Alr.Commands.Update;

package body Alr.Commands.Build is

   -------------
   -- Execute --
   -------------

   procedure Execute (Online : Boolean) is
      pragma Unreferenced (Online);
   begin
      Update.Execute;
      Compile.Execute;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
   begin
      Execute (Cmd.Online);
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("The build command ensures that the crate is built with "
               & "up-to-date dependencies. It first resolves the root crate "
               & "dependencies, updating the build aggregate project, and "
               & "fetching any missing dependencies. Finally, it invokes the "
               & "compile command."));

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
   begin
      GNAT.Command_Line.Define_Switch
        (Config,
         Cmd.Online'Access,
         "-o", "--online", "Update from online catalog before compiling");
   end Setup_Switches;

end Alr.Commands.Build;
