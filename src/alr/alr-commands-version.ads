with Alr.Platform;
with Alr.Utils;

package Alr.Commands.Version is

   type Command is new Commands.Command with null record;

   overriding
   procedure Execute (Cmd : in out Command);

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector;

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Shows alr diagnostics");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("");

   function Fingerprint return String;

private

   use Utils;

   -----------------
   -- Fingerprint --
   -----------------

   function Fingerprint return String is
       (To_Mixed_Case (Platform.Operating_System'Img) & " " &
        To_Mixed_Case (Platform.Word_Size'Img) & " " &
        To_Mixed_Case (Platform.Distribution'Img));

end Alr.Commands.Version;
