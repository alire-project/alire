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
   --  For this command we want the args after -- to pass them to gprinstall

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);

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
   end record;
end Alr.Commands.Install;
