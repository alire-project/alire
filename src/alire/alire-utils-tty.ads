with ANSI;

package Alire.Utils.TTY with Preelaborate is

   --  Color/Formatting related subprograms. These won't have any
   --  effect if a redirection of output is detected, or if global
   --  flag Simple_Logging.Is_TTY is false.

   --  Re-expose for clients
   package ANSI renames Standard.ANSI;

   --------------------
   -- Color enabling --
   --------------------

   function Color_Enabled return Boolean;

   procedure Disable_Color;
   --  Disables color/formatting output even when TTY is capable

   procedure Enable_Color (Force : Boolean := False);
   --  Prepares colors for the logging messages. Unless Force, will do nothing
   --  if a console redirection is detected.

   function Format (Text  : String;
                    Fore  : ANSI.Colors := ANSI.Default;
                    Back  : ANSI.Colors := ANSI.Default;
                    Style : ANSI.Styles := ANSI.Default)
                    return String;
   --  Wrap text with the appropriate ANSI sequences. Following text will be
   --  unaffected. Default colors are interpreted as no change of color (will
   --  result in no color sequences), not as setting the default color (which
   --  is always set after a color change).

   ------------------------
   -- Predefined formats --
   ------------------------

   function OK (Text : String) return String;
   --  Bold Light_Green

   function Emph (Text : String) return String;
   --  Something to highligth not negatively, bold cyan

   function Error (Text : String) return String;
   --  Bold Red

   function Warn (Text : String) return String;
   --  Bold Yellow

   function Bold (Text : String) return String;

   function Name (Crate : Crate_Name) return String;

   function Name (Text : String) return String;
   --  Bold Default for crate names

   function Description (Text : String) return String;
   --  Not bold cyan for crate descriptions

   function Version (Text : String) return String;
   --  For versions/version sets, bold magenta

private

   function OK (Text : String) return String is
     (Format (Text,
              Fore  => ANSI.Light_Green,
              Style => ANSI.Bright));

   function Emph (Text : String) return String is
     (Format (Text,
              Fore  => ANSI.Cyan,
              Style => ANSI.Bright));

   function Error (Text : String) return String is
     (Format (Text,
              Fore  => ANSI.Red,
              Style => ANSI.Bright));

   function Warn (Text : String) return String is
     (Format (Text,
              Fore  => ANSI.Yellow,
              Style => ANSI.Bright));

   function Bold (Text : String) return String is
     (Format (Text,
              Style => ANSI.Bright));

   function Name (Crate : Crate_Name) return String is
     (Name (+Crate));

   function Name (Text : String) return String is
     (Bold (Text));

   function Description (Text : String) return String is
     (Format (Text,
              Fore  => ANSI.Light_Cyan));

   function Version (Text : String) return String is
     (Format (Text,
              Fore  => ANSI.Magenta,
              Style => ANSI.Bright));

end Alire.Utils.TTY;
