with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with CLIC.Formatter;

with Alire.Selftest;

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

   -------------------
   -- Print_MD_Help --
   -------------------

   procedure Print_MD_Help is

      use Ada.Strings.Unbounded;

      Last_Group : Unbounded_String;

      procedure Put_MD_Command
        (Group : Unbounded_String;
         Cmd : not null CLIC.Subcommand.Command_Access) is
      begin

         if Group /= Last_Group then
            New_Line;
            Put_Line ("# " & To_String (Group) & " Commands");

            Last_Group := Group;
         end if;

         Put_Line ("## `alr " & Cmd.Name & "`");

         New_Line;
         Sub_Cmd.Display_Help (Cmd.Name);
         New_Line;

      end Put_MD_Command;

      procedure Put_MD_Topic
        (Topic : not null CLIC.Subcommand.Help_Topic_Access) is
      begin
         New_Line;
         Put_Line ("## " & Topic.Name);
         Sub_Cmd.Display_Help (Topic.Name);
         New_Line;
      end Put_MD_Topic;

   begin
      CLIC.Formatter.Enable_Markdown;

      Put_Line ("# Usage Help");
      Sub_Cmd.Display_Usage;

      Sub_Cmd.Iterate_Commands (Process => Put_MD_Command'Access);

      Put_Line ("# Topics");
      Sub_Cmd.Iterate_Topics (Process => Put_MD_Topic'Access);

   end Print_MD_Help;

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

      if Cmd.UTF_8_Test then
         Print_UTF_8_Sequence;
      end if;

      if Cmd.MD_Help then
         Print_MD_Help;
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

      Define_Switch (Config,
                     Cmd.UTF_8_Test'Access,
                     "", "--utf8",
                     "Print a known UTF-8 sequence");

      Define_Switch (Config,
                     Cmd.MD_Help'Access,
                     "", "--help-doc-markdown",
                     "Print a complete help page in markdown format");
   end Setup_Switches;

end Alr.Commands.Dev;
