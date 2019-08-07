pragma Warnings (Off);

with Alr.Bootstrap;
with Alr.Code;
with Alr.Parsers;
with Alr.Platform;
with Alr.Selftest;
with Alr.Spawn;

pragma Warnings (On);

package body Alr.Commands.Dev is

   ------------
   -- Custom --
   ------------

   procedure Custom is
   begin
      Trace.Always (+Parsers.Project_Versions ("abcd=1.0").Project);
   end Custom;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
   begin
      if Cmd.Custom then
         Custom;
      end if;

      if Cmd.Raise_Except then
         raise Program_Error with "Raising forcibly";
      end if;

      if Cmd.Self_Test then
         Selftest.Run;
      end if;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("Internal command for development help. Options and features"
               & " are not stable and may change without warning."));

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin
      Define_Switch (Config,
                     Cmd.Custom'Access,
                     "", "--custom",
                     "Execute current custom code");

      Define_Switch (Config,
                     Cmd.Raise_Except'Access,
                     "", "--raise",
                     "Raise an exception");

      Define_Switch (Config,
                     Cmd.Self_Test'Access,
                     "", "--test",
                     "Run self-tests");
   end Setup_Switches;

end Alr.Commands.Dev;
