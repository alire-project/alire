with AAA.Strings;

private with GNAT.Strings;

package Alr.Commands.Install is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("install");

   overriding
   function Switch_Parsing (This : Command)
                            return CLIC.Subcommand.Switch_Parsing_Kind
   is (CLIC.Subcommand.Parse_All);
   --  To keep things simple we don't forward switches to neither gprbuild nor
   --  gprinstall, and any scenarios have to be set up via environment e.g.,
   --  $ LIBRARY_TYPE=relocatable alr install <whatever>. We could improve on
   --  this down the line.

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);
   --  Will both build + gprinstall, to ensure both see the same environment

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector;

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Manage installation prefixes");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[switches] [crate[versions]]...");

private
   type Command is new Commands.Command with record
      Target : aliased GNAT.Strings.String_Access; -- Crate[version] to install
      Prefix : aliased GNAT.Strings.String_Access; -- Prefix for gprinstall
      This   : aliased Boolean := False; -- To install the current workspace
   end record;
end Alr.Commands.Install;
