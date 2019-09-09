with Ada.Command_Line;

with Alr.Selftest;

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

   overriding procedure Execute (Cmd : in out Command) is
   begin
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

      function Command_Line_Contains (Arg : String) return Boolean is
        (for some I in 1 .. Ada.Command_Line.Argument_Count =>
            Ada.Command_Line.Argument (I) = Arg);

   begin
      --  Special case to enable a simple test for switch redefinition.
      --  If exactly the second argument is "--check-switch-redefinition"
      --  (e.g., calling alr dev --check-switch-redefinition), this will result
      --  in alr erroring as expected. Two -1 and -2 variants are checked so
      --  both long and short switch clashes can be tested separately.
      if Command_Line_Contains ("--check-switch-redefinition-1") then
         Define_Switch (Config,
                        Cmd.Custom'Access,
                        "-h", "",
                        "Check for redefined switch");
      elsif Command_Line_Contains ("--check-switch-redefinition-2") then
         Define_Switch (Config,
                        Cmd.Custom'Access,
                        "", "--help",
                        "Check for redefined switch");
      end if;

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
