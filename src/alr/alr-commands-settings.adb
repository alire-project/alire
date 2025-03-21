with Alire.Settings.Edit;
with Alire.Root;

with CLIC.Config.Info;
with CLIC.Config.Edit;

package body Alr.Commands.Settings is

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      Enabled : Natural := 0;

      Lvl : constant Alire.Settings.Level := (if Cmd.Global
                                            then Alire.Settings.Global
                                            else Alire.Settings.Local);

      -------------------
      -- Check_Builtin --
      -------------------

      procedure Check_Builtin (Key : String) is
      begin
         --  Check if the setting is a builtin when --builtin is given
         if Cmd.Builtin and then not Alire.Settings.Is_Builtin (Key) then
            Reportaise_Wrong_Arguments
              ("'" & Key & "' is not a built-in setting");
         end if;

         --  Check if it is not a builtin when --builtin is not given
         if not Cmd.Builtin and then Alire.Settings.Is_Builtin (Key) then
            Trace.Warning ("'" & Key & "' is a built-in setting. "
                           & "Use --builtin to avoid this warning");
         end if;
      end Check_Builtin;
   begin
      Cmd.Forbids_Structured_Output;

      --  Check no multi-action
      Enabled := Enabled + (if Cmd.List then 1 else 0);
      Enabled := Enabled + (if Cmd.Get then 1 else 0);
      Enabled := Enabled + (if Cmd.Set then 1 else 0);
      Enabled := Enabled + (if Cmd.Unset then 1 else 0);
      Enabled := Enabled + (if Cmd.Builtins_Doc then 1 else 0);

      if Enabled > 1 then
         Reportaise_Wrong_Arguments ("Specify at most one subcommand");
      end if;

      if Enabled = 0 then
         --  The default command is --list
         Cmd.List := True;
      end if;

      if Cmd.Show_Origin and then not Cmd.List then
         Reportaise_Wrong_Arguments
           ("--show-origin only valid with --list");
      end if;

      if Cmd.Builtin and then not (Cmd.Get or else Cmd.Set or else Cmd.Unset)
      then
         Reportaise_Wrong_Arguments
           ("--builtin only valid with --get, --set or --unset");
      end if;

      if Cmd.Builtins_Doc then
         Alire.Settings.Edit.Print_Builtins_Doc;
         return;
      end if;

      if not Cmd.Global and then not Alire.Root.Current.Is_Valid then
         Reportaise_Command_Failed
           ("Not in an Alire project directory." &
              " Use --global to edit the global settings.");
      end if;

      if Cmd.List then
         case Args.Count is
            when 0 =>
               Trace.Always
                 (CLIC.Config.Info.List
                    (Alire.Settings.DB.all,
                     Filter => ".*",
                     Show_Origin => Cmd.Show_Origin).Flatten (ASCII.LF));
            when 1 =>
               Trace.Always
                 (CLIC.Config.Info.List
                    (Alire.Settings.DB.all,
                     Filter => Args.First_Element,
                     Show_Origin => Cmd.Show_Origin).Flatten (ASCII.LF));
            when others =>
               Reportaise_Wrong_Arguments
                 ("List expects at most one argument");
         end case;

      elsif Cmd.Get then
         if Args.Count /= 1 then
            Reportaise_Wrong_Arguments ("Get expects one argument");
         end if;

         declare
            Key : constant String := Args.First_Element;
         begin
            if not CLIC.Config.Is_Valid_Config_Key (Key) then
               Reportaise_Wrong_Arguments ("Invalid setting key '" &
                                          Key & "'");
            end if;

            Check_Builtin (Key);

            if Alire.Settings.DB.Defined (Key) then
               Trace.Always
                 (Alire.Settings.DB.Get_As_String (Key));
            else
               Reportaise_Command_Failed ("Setting key '" &
                                         Key &
                                         "' is not defined");
            end if;
         end;
      elsif Cmd.Set then
         if Args.Count /= 2 then
            Reportaise_Wrong_Arguments ("Set expects two arguments");
         end if;

         declare
            Key : constant String := Args.Element (1);
            Val : constant String := Args.Element (2);
         begin

            if not CLIC.Config.Is_Valid_Config_Key (Key) then
               Reportaise_Wrong_Arguments ("Invalid setting key '" &
                 Key & "'");
            end if;

            Check_Builtin (Key);

            --  Check explicitly for booleans to store the proper TOML type
            --  regardless of the capitalization used by the user.
            if Is_Boolean (Val) then
               Alire.Settings.Edit.Set_Boolean
                 (Lvl,
                  Key, Boolean'Value (Val),
                  Check => Alire.Settings.Edit.Valid_Builtin_Check (Lvl));
            else
               Alire.Settings.Edit.Set
                 (Lvl,
                  Key, Val,
                  Check => Alire.Settings.Edit.Valid_Builtin_Check (Lvl));
            end if;
         end;

      elsif Cmd.Unset then
         if Args.Count /= 1 then
            Reportaise_Wrong_Arguments ("Unset expects one argument");
         end if;

         declare
            Key : constant String := Args.Element (1);
         begin
            if not CLIC.Config.Is_Valid_Config_Key (Key) then
               Reportaise_Wrong_Arguments ("Invalid setting key '" &
                 Key & "'");
            end if;

            Check_Builtin (Key);

            if not CLIC.Config.Edit.Unset
              (Alire.Settings.Edit.Filepath (Lvl), Key)
            then
               Reportaise_Command_Failed ("Cannot unset setting key");
            end if;
         end;
      end if;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector is
     (AAA.Strings.Empty_Vector
      .Append ("Provides a command line interface to the Alire settings" &
                 " files.")
      .New_Line
      .Append ("Settings names (keys) can use lowercase and uppercase" &
                 " alphanumeric characters")
      .Append ("from the Latin alphabet. Underscores and dashes can also be" &
                 " used except as")
      .Append ("first or last character. Dot '.' is used to specify" &
                 " sub-categories, e.g.")
      .Append ("'user.name' or 'user.email'.")
      .New_Line

      .Append ("Option values can be integers, float, Boolean (true or" &
                 " false) or strings. The")
      .Append ("type detection is automatic, e.g. 10 is integer, 10.1 is" &
                 " float, true is")
      .Append ("Boolean. You can force a value to be set a string by using" &
                 " double-quotes, e.g.")
      .Append ("""10.1"" or ""true"". Extra type checking is used for" &
                 " built-in options (see below).")
      .New_Line
      .Append ("Built-in settings:")
      .New_Line
      .Append (Alire.Settings.Edit.Builtins_Info));

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
   begin
      Define_Switch
        (Config      => Config,
         Output      => Cmd.List'Access,
         Long_Switch => "--list",
         Help        => "List settings options");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Show_Origin'Access,
         Long_Switch => "--show-origin",
         Help        => "Show origin of settings values in --list");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Get'Access,
         Long_Switch => "--get",
         Help        => "Print value of a setting option");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Set'Access,
         Long_Switch => "--set",
         Help        => "Set a setting option");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Unset'Access,
         Long_Switch => "--unset",
         Help        => "Unset a setting option");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Global'Access,
         Long_Switch => "--global",
         Help        => "Set and Unset global settings instead" &
                         " of the local one");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Builtins_Doc'Access,
         Long_Switch => "--builtins-doc",
         Help        => "Print Markdown list of built-in settings");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Builtin'Access,
         Long_Switch => "--builtin",
         Help        => "Operate on built-in settings only");

   end Setup_Switches;

end Alr.Commands.Settings;
