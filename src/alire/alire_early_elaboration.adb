with Ada.Text_IO;

with Alire;

with GNAT.Command_Line;
with GNAT.OS_Lib;

with Interfaces.C_Streams;

with Simple_Logging.Filtering;

package body Alire_Early_Elaboration is

   ----------------
   -- Add_Scopes --
   ----------------

   procedure Add_Scopes (Debug_Arg : String) is
      --  Receives as-is the --debug/-d[ARG] argument. This is a list of
      --  optionally comma-separated, plus/minus prefixed substrings that will
      --  be used for filtering against the enclosing entity/source location.
      --  Example whitelisting argument: +commands,-search
      --  Example blacklisting argument: -commands,+search
      --  The first sign puts the filter in (-) blacklist / (+) whitelist mode.
      --  In whitelist mode, only the given substrings are logged, unless later
      --  added as exception. E.g., in the "+commands,-search" example, only
      --  commands traces would be logged (because of whitelist mode), except
      --  the ones for the search command (because given as an exception).
      --  In the "-commands,+search" example for blacklist mode, everything but
      --  command traces would be logged, but search command traces would be
      --  logged because that's the exception.

      --  Once scopes are used, we activate logging of enclosing entity and
      --  location to provide full logging information.

   begin
      if not Simple_Logging.Filtering.Add_From_String (Debug_Arg,
                                                       Say => True)
      then
         --  Bypass debug channel, which was not entirely set up.
         --  Otherwise we get unwanted location/entity info already.
         Ada.Text_IO.Put_Line ("ERROR: Invalid logging filters.");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
   end Add_Scopes;

   ----------------------------
   -- Early_Switch_Detection --
   ----------------------------

   procedure Early_Switch_Detection is
      use GNAT.Command_Line;

      --------------------
      -- Check_Switches --
      --------------------

      procedure Check_Switches is

         ----------------------
         -- Check_Long_Debug --
         ----------------------
         --  Take care manually of the -debug[ARG] optional ARG, since the
         --  simple Getopt below doesn't for us:
         procedure Check_Long_Debug (Switch : String) is
            Target : constant String := "--debug";
         begin
            if Switch'Length >= Target'Length and then
              Switch (Switch'First ..
                        Switch'First + Target'Length - 1) = Target
            then
               Switch_D := True;
               Add_Scopes
                 (Switch (Switch'First + Target'Length .. Switch'Last));
            end if;
         end Check_Long_Debug;

      begin
         loop
            --  We use the simpler Getopt form to avoid built-in help and other
            --  shenanigans.
            case Getopt ("* d? --debug? q v") is
               when ASCII.NUL =>
                  exit;
               when '*' =>
                  Check_Long_Debug (Full_Switch);
               when 'd' =>
                  Switch_D := True;
                  Add_Scopes (Parameter);
               when 'q' =>
                  Switch_Q := True;
               when 'v' =>
                  if Switch_V and then not Switch_VV then
                     Switch_VV := True;
                     Switch_V  := False;
                  elsif not (Switch_V or else Switch_VV) then
                     Switch_V := True;
                  else
                     Alire.Trace.Error ("Only one or two -v allowed");
                     GNAT.OS_Lib.OS_Exit (1);
                  end if;
               when others =>
                  null;
            end case;
         end loop;
      exception
         when Exit_From_Command_Line =>
            --  Something unexpected happened but it will be properly dealt
            --  with later on, in the regular command-line parser.
            null;
      end Check_Switches;

   begin
      Check_Switches;

      --  Exclusivity check
      if (Switch_Q and Switch_V) or (Switch_Q and Switch_VV)
      then
         Alire.Trace.Error
           ("Use only one of -q or -v");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      --  Level setting
      if Switch_VV then
         Alire.Log_Level := Simple_Logging.Debug;
      elsif Switch_V then
         Alire.Log_Level := Simple_Logging.Detail;
      elsif Switch_Q then
         Alire.Log_Level := Simple_Logging.Error;
      end if;

      --  Debug channel
      if Switch_D then
         Alire.Log_Debug := True;
      end if;
   end Early_Switch_Detection;

   -------------------
   -- TTY_Detection --
   -------------------

   procedure TTY_Detection is
      use Interfaces.C_Streams;
   begin
      Simple_Logging.Is_TTY := isatty (fileno (stdout)) /= 0;
   end TTY_Detection;

begin
   TTY_Detection;
   Early_Switch_Detection;
end Alire_Early_Elaboration;
