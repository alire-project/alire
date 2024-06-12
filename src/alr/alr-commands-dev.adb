with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Finalization;

with Alire.Selftest;
with Alire.Utils;

package body Alr.Commands.Dev is

   ------------
   -- Custom --
   ------------

   procedure Custom is
   begin
      null;
   end Custom;

   --------------------------
   -- Print_UTF_8_Sequence --
   --------------------------

   procedure Print_UTF_8_Sequence is
      use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
   begin
      --  When compiled with -gnatW8, the following should produce valid UTF-8
      Trace.Always (Encode ("ⓘ✓"));
   end Print_UTF_8_Sequence;

   -----------------------------
   -- Raise_From_Finalization --
   -----------------------------

   procedure Raise_From_Finalization is
      type Ctrl is new Ada.Finalization.Controlled with null record;

      overriding
      procedure Finalize (This : in out Ctrl) is
      begin
         raise Program_Error with "Raising forcibly from finalization";
      exception
         when E : others =>
            Alire.Utils.Finalize_Exception (E);
      end Finalize;

      Test : Ctrl;
      pragma Unreferenced (Test);
   begin
      null;
   end Raise_From_Finalization;

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

      if Cmd.Error then
         Alire.Recoverable_Program_Error ("Forced error");
      end if;

      if Cmd.Raise_Except then
         raise Program_Error with "Raising forcibly";
      end if;

      if Cmd.Raise_Final then
         Raise_From_Finalization;
      end if;

      if Cmd.Self_Test then
         Alire.Selftest.Run;
      end if;

      if Cmd.UTF_8_Test then
         Print_UTF_8_Sequence;
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
                     Cmd.Error'Access,
                     "", "--error",
                     "Program error report");

      Define_Switch (Config,
                     Cmd.Raise_Except'Access,
                     "", "--raise",
                     "Raise an exception");

      Define_Switch (Config,
                     Cmd.Raise_Final'Access,
                     "", "--raise-finalization",
                     "Raise an exception from a finalization procedure");

      Define_Switch (Config,
                     Cmd.Self_Test'Access,
                     "", "--test",
                     "Run self-tests");

      Define_Switch (Config,
                     Cmd.UTF_8_Test'Access,
                     "", "--utf8",
                     "Print a known UTF-8 sequence");
   end Setup_Switches;

end Alr.Commands.Dev;
