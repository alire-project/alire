with Alr.Platform;
with Alr.Utils;

package Alr.Commands.Version is

   type Command is new Commands.Command with null record;

   overriding procedure Execute (Cmd : in out Command);

   overriding function Short_Description (Cmd : Command) return String is
     ("Shows alr diagnostics");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is ("");

   function Fingerprint return String;

   function Git_Tag return String;
   --  Returns latest annotated tag in alr sources (should be the alr version)

private

   use Utils;

   -----------------
   -- Fingerprint --
   -----------------

   function Fingerprint return String is
       (To_Mixed_Case (Platform.Operating_System'Img) & " " &
        To_Mixed_Case (Platform.Word_Size'Img) & " " &
        To_Mixed_Case (Platform.Distribution'Img) & " " &
        To_Mixed_Case (Platform.Distro_Version'Img) & " " &
        To_Mixed_Case (Platform.Compiler'Img));

end Alr.Commands.Version;
