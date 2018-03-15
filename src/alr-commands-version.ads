with Alr.Platforms;
with Alr.Platforms.Current;
with Alr.Utils;

package Alr.Commands.Version is

   type Command is new Commands.Command with null record;

   overriding procedure Execute (Cmd : in out Command);

   overriding function Short_Description (Cmd : Command) return String is
     ("Shows alr diagnostics");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is ("");

   function Fingerprint return String;

private

   use Utils;

   -----------------
   -- Fingerprint --
   -----------------

   function Fingerprint return String is
       (To_Mixed_Case (Platforms.Current.Instance.Operating_System'Img) & " " &
        To_Mixed_Case (Platforms.Word_Size'Img) & " " &
        To_Mixed_Case (Platforms.Current.Instance.Distribution'Img) & " " &
        To_Mixed_Case (Platforms.Current.Instance.Distro_Version'Img) & " " &
        Platforms.Compiler'Img);

end Alr.Commands.Version;
