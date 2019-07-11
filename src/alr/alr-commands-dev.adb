pragma Warnings (Off);

with Ada.Command_Line;

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

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin
      --  Special case to enable a simple test for switch redefinition.
      --  If exactly the second argument is "--check-switch-redefinition"
      --  (e.g., calling alr dev --check-switch-redefinition), this will result
      --  in alr erroring as expected. Two -1 and -2 variants are checked so
      --  both long and short switch clashes can be tested separately.
      if Ada.Command_Line.Argument_Count = 2 then
         if Ada.Command_Line.Argument (2) = "--check-switch-redefinition-1"
         then
            Define_Switch (Config,
                           Cmd.Custom'Access,
                           "-h", "",
                           "Check for redefined switch");
         elsif Ada.Command_Line.Argument (2) = "--check-switch-redefinition-2"
         then
            Define_Switch (Config,
                           Cmd.Custom'Access,
                           "", "--help",
                           "Check for redefined switch");
         end if;
      end if;

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
