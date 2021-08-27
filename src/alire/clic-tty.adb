with Interfaces.C_Streams;
with Simple_Logging; use Simple_Logging;
with Simple_Logging.Decorators;

package body CLIC.TTY is

   use all type ANSI.Colors;
   use all type ANSI.Styles;

   Use_Color : Boolean := False; -- Err on the safe side

   function Regular_Decorator (Level   : Simple_Logging.Levels;
                               Message : String) return String;

   function Verbose_Decorator (Level   : Simple_Logging.Levels;
                               Message : String) return String;

   Disabled_By_User : Boolean := False;

   -----------------------
   -- Force_Disable_TTY --
   -----------------------

   procedure Force_Disable_TTY is
   begin
      Disabled_By_User := True;
   end Force_Disable_TTY;

   ---------------
   -- Check_TTY --
   ---------------

   function Check_TTY return Boolean is
      use Interfaces.C_Streams;
   begin
      return isatty (fileno (stdin)) /= 0;
   end Check_TTY;

   ------------
   -- Is_TTY --
   ------------

   function Is_TTY return Boolean
   is (if Disabled_By_User then False else Check_TTY);

   -------------------
   -- Color_Enabled --
   -------------------

   function Color_Enabled return Boolean is (Use_Color);

   -------------------
   -- Disable_Color --
   -------------------

   procedure Disable_Color is
   begin
      Use_Color := False;
   end Disable_Color;

   ------------------
   -- Enable_Color --
   ------------------

   procedure Enable_Color (Force : Boolean := False) is
   begin

      --  Enable when appropriate

      if Force or else Is_TTY then
         Use_Color := True;
         Simple_Logging.Debug ("Color output enabled");
      else
         Simple_Logging.Debug ("Color output was requested but not enabled:"
                               & " force=" & Force'Img
                               & "; Is_TTY=" & Is_TTY'Img);
      end if;

      --  Set debug colors. When Detail/Debug are also enabled, we add the
      --  "info:" prefix to otherwise normal output, so it's seen more easily.

      if Use_Color then
         case Simple_Logging.Level is
            when Always .. Info =>
               Decorators.Level_Decorator :=
                 Regular_Decorator'Access;
            when others =>
               Decorators.Level_Decorator :=
                 Verbose_Decorator'Access;
         end case;
      end if;
   end Enable_Color;

   ------------
   -- Format --
   ------------

   function Format (Text  : String;
                    Fore  : ANSI.Colors := ANSI.Default;
                    Back  : ANSI.Colors := ANSI.Default;
                    Style : ANSI.Styles := ANSI.Default)
                    return String
   is
      use ANSI;
   begin
      if not Use_Color then
         return Text;
      end if;

      return
        ((if Fore  /= Default then Foreground (Fore) else "")
         & (if Back /= Default then Background (Fore) else "")
         & (if Style /= Default then ANSI.Style (Style, On) else "")
         & Text
         & (if Fore  /= Default then Foreground (Default) else "")
         & (if Back /= Default then Background (Default) else "")
         & (if Style /= Default then ANSI.Style (Style, Off) else ""));
   end Format;

   -----------------------
   -- Regular_Decorator --
   -----------------------

   function Regular_Decorator (Level   : Simple_Logging.Levels;
                               Message : String) return String is
     (case Level is
         when Info   => Message,
         when others => Verbose_Decorator (Level, Message));

   -----------------------
   -- Verbose_Decorator --
   -----------------------

   function Verbose_Decorator (Level : Simple_Logging.Levels;
                               Message : String) return String
   is
      use ANSI;
   begin
      return
     (case Level is
         when Always  => Message,
         when Error   =>
            ANSI.Wrap (Text       => "error:",
                       Style      => Bright,
                       Foreground => ANSI.Foreground (Red)) & " " & Message,
         when Warning =>
            ANSI.Wrap (Text       => "warn:",
                       Style      => Bright,
                       Foreground => ANSI.Foreground (Yellow)) & " " & Message,
         when Info    =>
            ANSI.Wrap (Text       => "info:",
                       Style      => Bright,
                       Foreground => ANSI.Foreground (Green)) & " " & Message,
         when Detail  =>
            ANSI.Wrap (Text       => "detail:",
                       Style      => Bright,
                       Foreground => ANSI.Foreground (Cyan)) & " " & Message,
         when Debug   =>
            ANSI.Wrap (Text       => "debug:",
                       Style      => Default,
                       Foreground => ANSI.Foreground (Grey)) & " "
            & ANSI.Wrap (Text       => Message,
                         Style      => Dim));
   end Verbose_Decorator;

end CLIC.TTY;
