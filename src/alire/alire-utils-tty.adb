with ANSI;

with Simple_Logging.Decorators;

package body Alire.Utils.TTY is

   use all type ANSI.Colors;
   use all type ANSI.Styles;

   type States is (Disabled, Enabled, Default);

   Status : States := Default;

   function Regular_Decorator (Level   : Simple_Logging.Levels;
                               Message : String) return String;

   function Verbose_Decorator (Level   : Simple_Logging.Levels;
                               Message : String) return String;

   ---------------
   -- Use_Color --
   ---------------

   function Use_Color return Boolean is
     (case Status is
         when Enabled  => True,
         when Disabled => False,
         when Default  => Simple_Logging.Is_TTY);

   -------------------
   -- Disable_Color --
   -------------------

   procedure Disable_Color is
   begin
      Status := Disabled;
   end Disable_Color;

   ------------------
   -- Enable_Color --
   ------------------

   procedure Enable_Color (Force : Boolean := False) is
   begin

      --  Enable when appropriate

      if Force or else Simple_Logging.Is_TTY then
         Status := Enabled;
      end if;

      --  Set debug colors. When Detail/Debug are also enabled, we add the
      --  "info:" prefix to otherwise normal output, so it's seen more easily.

      if Use_Color then
         case Log_Level is
            when Always .. Info =>
               Simple_Logging.Decorators.Level_Decorator :=
                 Regular_Decorator'Access;
            when others =>
               Simple_Logging.Decorators.Level_Decorator :=
                 Verbose_Decorator'Access;
         end case;
      end if;
   end Enable_Color;

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
                               Message : String) return String is
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

end Alire.Utils.TTY;
