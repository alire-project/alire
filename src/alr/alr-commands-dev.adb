with Alire.Selftest;

package body Alr.Commands.Dev is

   ------------
   -- Custom --
   ------------

   procedure Custom is
   begin
      null;
   end Custom;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
   begin
      if Args.Count /= 0 then
         Reportaise_Wrong_Arguments (Cmd.Name & " doesn't take arguments");
      end if;

      if Cmd.Custom then
         Custom;
      end if;

      if Cmd.Filtering then
         Trace.Debug ("In dev --filter");
      end if;

      if Cmd.Raise_Except then
         raise Program_Error with "Raising forcibly";
      end if;

      if Cmd.Self_Test then
         Alire.Selftest.Run;
      end if;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector is
     (AAA.Strings.Empty_Vector
      .Append ("Internal command for development help. Options and features"
               & " are not stable and may change without warning."));

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;

   begin
      Define_Switch (Config,
                     Cmd.Custom'Access,
                     "", "--custom",
                     "Execute current custom code");

      Define_Switch (Config,
                     Cmd.Filtering'Access,
                     "", "--filter",
                     "Used by scope filtering test");

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
