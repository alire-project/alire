with AAA.Strings;

with Ada.Directories;

with Alire.Features;
with Alire.Settings.Edit.Early_Load;
with Alire.Version;

with GNAT.Command_Line;
with GNAT.OS_Lib;
with GNAT.IO;

with Interfaces.C_Streams;

with r;

with Simple_Logging.Filtering;

package body Alire_Early_Elaboration is

   -----------------
   -- Early_Error --
   -----------------

   procedure Early_Error (Text : String) is
   begin
      GNAT.IO.Put_Line ("ERROR: " & Text);
      GNAT.OS_Lib.OS_Exit (1);
   end Early_Error;

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
         Early_Error ("Invalid logging filters.");
      end if;
   end Add_Scopes;

   ----------------------------
   -- Early_Switch_Detection --
   ----------------------------

   procedure Early_Switch_Detection is
      use GNAT.Command_Line;

      Subcommand_Seen : Boolean := False;

      --------------------
      -- Check_Switches --
      --------------------

      procedure Check_Switches is

         ---------------------------
         -- Settings_Switch_Error --
         ---------------------------

         procedure Settings_Switch_Error (Switch : String) is
         begin
            GNAT.IO.Put_Line
               ("ERROR: Switch " & Switch & " requires argument (global).");
            Early_Error ("try ""alr --help"" for more information.");
         end Settings_Switch_Error;

         ---------------------
         -- Set_Config_Path --
         ---------------------

         procedure Set_Config_Path (Switch, Path : String) is
            package Adirs renames Ada.Directories;
         begin
            if Path = "" then
               Settings_Switch_Error (Switch);
            elsif not Adirs.Exists (Path) then
               Early_Error
                 ("Invalid non-existing configuration path: " & Path);
            elsif Adirs.Kind (Path) not in Adirs.Directory then
               Early_Error
                 ("Given configuration path is not a directory: " & Path);
            else
               Alire.Settings.Edit.Set_Path (Adirs.Full_Name (Path));
            end if;
         end Set_Config_Path;

         -----------------------------
         -- Check_Config_Deprecated --
         -----------------------------

         use type Alire.Version.Semver.Version;
         Config_Deprecated : constant Boolean
           := Alire.Version.Current >= Alire.Features.Config_Deprecated;

         procedure Check_Config_Deprecated is
         begin
            if Config_Deprecated then
               Early_Error
                 ("--config is deprecated, use --settings instead");
            end if;
         end Check_Config_Deprecated;

         ----------------
         -- Check_Seen --
         ----------------

         Settings_Seen : Boolean := False;

         procedure Check_Settings_Seen is
         begin
            if Settings_Seen then
               Early_Error
                 ("Only one of -s, --settings"
                  & (if not Config_Deprecated
                    then ", -c, --config"
                    else "")
                  & " allowed");
            else
               Settings_Seen := True;
            end if;
         end Check_Settings_Seen;

         -----------------------
         -- Check_Long_Switch --
         -----------------------
         --  Take care manually of the -debug[ARG] optional ARG, since the
         --  simple Getopt below doesn't for us:
         procedure Check_Long_Switch (Switch : String) is
            use AAA.Strings;
         begin
            if Switch (Switch'First) /= '-' then
               Subcommand_Seen := True;
            elsif Has_Prefix (Switch, "--debug") then
               Switch_D := True;
               Add_Scopes (Tail (Switch, "="));
            elsif Has_Prefix (Switch, "--config") then
               Check_Config_Deprecated;
               Check_Settings_Seen;
               Set_Config_Path (Switch, Tail (Switch, "="));
            elsif Has_Prefix (Switch, "--settings") then
               Check_Settings_Seen;
               Set_Config_Path (Switch, Tail (Switch, "="));
            end if;
         end Check_Long_Switch;

         Option : Character;

      begin
         loop
            --  We use the simpler Getopt form to avoid built-in help and other
            --  shenanigans.
            Option := Getopt ("* d? --debug? q v c= --config= s= --settings=");
            case Option is
               when ASCII.NUL =>
                  exit;
               when '*' =>
                  if not Subcommand_Seen then
                     Check_Long_Switch (Full_Switch);
                  end if;
               when 'c' | 's' =>
                  if not Subcommand_Seen then
                     Check_Settings_Seen;
                     if Option = 'c' then
                        Check_Config_Deprecated;
                     end if;
                     Set_Config_Path ("-" & Option, Parameter);
                  end if;
               when 'd' =>
                  if not Subcommand_Seen then
                     Switch_D := True;
                     Add_Scopes (Parameter);
                  end if;
               when 'q' =>
                  if not Subcommand_Seen then
                     Switch_Q := True;
                  end if;
               when 'v' =>
                  if not Subcommand_Seen then
                     if Switch_V and then not Switch_VV then
                        Switch_VV := True;
                        Switch_V  := False;
                     elsif not (Switch_V or else Switch_VV) then
                        Switch_V := True;
                     else
                        Alire.Trace.Error ("Only one or two -v allowed");
                        GNAT.OS_Lib.OS_Exit (1);
                     end if;
                  end if;
               when others =>
                  null;
            end case;
         end loop;
      exception
         when GNAT.Command_Line.Invalid_Parameter =>
            if Option in 'c' | 's' then
               Settings_Switch_Error ("-" & Option);
            end if;
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

      --  Load config ASAP
      Alire.Settings.Edit.Early_Load.Load_Settings;
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
   Simple_Logging.Stdout_Level := Simple_Logging.Info;
   --  Display warnings and errors to stderr

   TTY_Detection;
   Early_Switch_Detection;

   r.Init; -- Register all embedded resources
end Alire_Early_Elaboration;
