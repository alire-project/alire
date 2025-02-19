with AAA.Strings;

private with GNAT.Strings;

package Alr.Commands.Withing is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("with");

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

   overriding function Short_Description (Cmd : Command) return String is
     ("Add or remove dependencies");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is
     ("[{ [--del] <crate>[versions]..."
      & " | --from <gpr_file>..."
      & " | <crate>[versions] --use <path> "
      & "[--commit REF] [--branch NAME] [--subdir REL_PATH]} ]"
      & " | --solve | --tree | --versions");

private

   type Command is new Commands.Command with record
      Branch   : aliased GNAT.Strings.String_Access;
      Commit   : aliased GNAT.Strings.String_Access;
      Subdir   : aliased GNAT.Strings.String_Access;
      Del      : aliased Boolean := False;
      From     : aliased Boolean := False;
      Graph    : aliased Boolean := False;
      Solve    : aliased Boolean := False;
      Tree     : aliased Boolean := False;
      URL      : aliased GNAT.Strings.String_Access;
      Versions : aliased Boolean := False;
   end record;

end Alr.Commands.Withing;
