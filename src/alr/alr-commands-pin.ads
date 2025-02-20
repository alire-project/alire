with AAA.Strings;

with GNAT.Strings;

package Alr.Commands.Pin is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("pin");

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector;

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Pin dependencies to exact versions");

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[[crate[=<version>]]"
       & " | crate --use=<path>"
       & " [--commit=REF] [--branch=NAME] [--subdir=REL_PATH]"
       & " | --all]");

private

   type Command is new Commands.Command with record
      Branch  : aliased GNAT.Strings.String_Access;
      Commit  : aliased GNAT.Strings.String_Access;
      Subdir  : aliased GNAT.Strings.String_Access;
      Pin_All : aliased Boolean;
      Unpin   : aliased Boolean;
      URL     : aliased GNAT.Strings.String_Access;
   end record;

end Alr.Commands.Pin;
