with AAA.Strings;

with Alire.Platforms.Current;

package Alr.Commands.Version is

   package Platform renames Alire.Platforms.Current;

   type Command is new Commands.Command with null record;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("version");

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector;

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Show detailed version, configuration, and environment information");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("");

   function Fingerprint return String;

   procedure Print_Version;
   --  Print a one-liner version report

private

   -----------------
   -- Fingerprint --
   -----------------

   function Fingerprint return String is
       (AAA.Strings.To_Mixed_Case (Platform.Operating_System'Img) & " " &
        AAA.Strings.To_Mixed_Case (Platform.Word_Size'Img) & " " &
        AAA.Strings.To_Mixed_Case (Platform.Distribution'Img));

end Alr.Commands.Version;
