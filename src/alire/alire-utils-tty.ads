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

   function Info (Text : String := "") return String;
   --  Prepends Text with a Emph ("🛈") or "Note: " if no tty color enabled

   function Success (Text : String := "") return String;
   --  Prepends Text (in normal formatting) with a green check mark, or a
   --  simple Success: text if no tty or color enabled.

   function OK (Text : String) return String;
   --  Bold Light_Green

   function Emph (Text : String) return String;
   --  Something to highlight not negatively, bold cyan

   function Error (Text : String) return String;
   --  Bold Red

   function Warn (Text : String) return String;
   --  Bold Yellow

   function Bold (Text : String) return String;

   function Dim (Text : String) return String;

   function Italic (Text : String) return String;

   function Underline (Text : String) return String;

   function Name (Crate : Crate_Name) return String;

   function Name (Text : String) return String;
   --  Bold Default for crate names

   function Description (Text : String) return String;
   --  Not bold cyan for crate descriptions

   function Terminal (Text : String) return String;
   --  For showing commands that the user can run; mimics old amber displays.

   function URL (Text : String) return String;

   function Version (Text : String) return String;
   --  For versions/version sets, bold magenta

   ----------------------
   -- Purpose-specific --
   ----------------------

   function Alr return String is (Terminal ("alr"));

private

   function Info (Text : String := "") return String is
     (if Color_Enabled and then Is_TTY
      then Emph ("ⓘ") & " " & Text
      else "Note: " & Text);

   function Success (Text : String := "") return String is
     (if Color_Enabled and then Is_TTY
      then OK ("✓") & " " & Text
      else "Success: " & Text);

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

   function Dim (Text : String) return String is
     (Format (Text,
              Style => ANSI.Dim));

   function Italic (Text : String) return String is
     (Format (Text,
              Style => ANSI.Italic));

   function Underline (Text : String) return String is
     (Format (Text,
              Style => ANSI.Underline));

   function Name (Crate : Crate_Name) return String is
     (Name (+Crate));

   function Name (Text : String) return String is
     (Bold (Text));

   function Description (Text : String) return String is
     (Format (Text,
              Fore  => ANSI.Light_Cyan));

   function Terminal (Text : String) return String is
     (ANSI.Color_Wrap (Text, ANSI.Palette_Fg (5, 3, 0)));

   function URL (Text : String) return String renames Version;

   function Version (Text : String) return String is
     (Format (Text,
              Fore  => ANSI.Magenta,
              Style => ANSI.Bright));

end Alire.Utils.TTY;
