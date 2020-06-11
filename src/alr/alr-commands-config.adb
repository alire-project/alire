with Alire.Config.Edit;
with Alire.Root;

package body Alr.Commands.Config is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      Enabled : Natural := 0;

      Lvl : constant Alire.Config.Level := (if Cmd.Global
                                            then Alire.Config.Global
                                            else Alire.Config.Local);
   begin

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

      if Cmd.Builtins_Doc then
         Alire.Config.Print_Builtins_Doc;
         return;
      end if;

      if not Cmd.Global and then not Alire.Root.Current.Is_Valid then
         Reportaise_Command_Failed
           ("Not in an Alire project directory." &
              " Use --global to edit the global configuration.");
      end if;

      if Cmd.List then
         case Num_Arguments is
            when 0 =>
               Trace.Always (Alire.Config.List
                             (Filter => "*",
                              Show_Origin => Cmd.Show_Origin));
            when 1 =>
               Trace.Always (Alire.Config.List
                             (Filter => Argument (1),
                              Show_Origin => Cmd.Show_Origin));
            when others =>
               Reportaise_Wrong_Arguments
                 ("List expects at most one argument");
         end case;

      elsif Cmd.Get then
         if Num_Arguments /= 1 then
            Reportaise_Wrong_Arguments ("Unset expects one argument");
         end if;

         if not Alire.Config.Is_Valid_Config_Key (Argument (1)) then
            Reportaise_Wrong_Arguments ("Invalid configration key '" &
                                          Argument (1) & "'");
         end if;

         if Alire.Config.Defined (Argument (1)) then
            Trace.Always (Alire.Config.Get_As_String (Argument (1)));
         else
            Reportaise_Command_Failed ("Configuration key '" & Argument (1) &
                                         "' is not defined");
         end if;
      elsif Cmd.Set then
         if Num_Arguments /= 2 then
            Reportaise_Wrong_Arguments ("Set expects two arguments");
         end if;

         declare
            Key : constant String := Argument (1);
            Val : constant String := Argument (2);
         begin

            if not Alire.Config.Is_Valid_Config_Key (Key) then
               Reportaise_Wrong_Arguments ("Invalid configration key '" &
                 Key & "'");
            end if;

            Alire.Config.Edit.Set (Alire.Config.Filepath (Lvl), Key, Val);
         end;

      elsif Cmd.Unset then
         if Num_Arguments /= 1 then
            Reportaise_Wrong_Arguments ("Unset expects one argument");
         end if;

         declare
            Key : constant String := Argument (1);
         begin
            if not Alire.Config.Is_Valid_Config_Key (Key) then
               Reportaise_Wrong_Arguments ("Invalid configration key '" &
                 Key & "'");
            end if;

            Alire.Config.Edit.Unset (Alire.Config.Filepath (Lvl), Key);
         end;
      end if;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("Provides a command line interface to the Alire configuration" &
                 " option files.")
      .New_Line
      .Append ("Option names (keys) can use lowercase and uppercase" &
                 " alphanumeric characters")
      .Append ("from the latin alphabet. Underscores and dashes can also be" &
                 " used except as")
      .Append ("first or last character. Dot '.' is used to specify" &
                 " sub-categories, e.g.")
      .Append ("'user.name' or 'user.email'.")
      .New_Line

      .Append ("Option values can be integers, float, boolean (true or" &
                 " false) or strings. The")
      .Append ("type detection is automatic, e.g. 10 is integer, 10.1 is" &
                 " float, true is")
      .Append ("boolean. You can force a value to be set a string by using" &
                 " double-quotes, e.g.")
      .Append ("""10.1"" or ""true"". Extra type checking is used for" &
                 " buitl-in options (see below).")
      .New_Line
      .Append ("Built-in configuration options:")
      .New_Line
      .Append (Alire.Config.Builtins_Info));

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration) is
   begin
      GNAT.Command_Line.Define_Switch
        (Config      => Config,
         Output      => Cmd.List'Access,
         Long_Switch => "--list",
         Help        => "List configuration options");

      GNAT.Command_Line.Define_Switch
        (Config      => Config,
         Output      => Cmd.Show_Origin'Access,
         Long_Switch => "--show-origin",
         Help        => "Show origin of configuration values in --list");

      GNAT.Command_Line.Define_Switch
        (Config      => Config,
         Output      => Cmd.Get'Access,
         Long_Switch => "--get",
         Help        => "Print value of a configuration option");

      GNAT.Command_Line.Define_Switch
        (Config      => Config,
         Output      => Cmd.Set'Access,
         Long_Switch => "--set",
         Help        => "Set a configuration option");

      GNAT.Command_Line.Define_Switch
        (Config      => Config,
         Output      => Cmd.Unset'Access,
         Long_Switch => "--unset",
         Help        => "Unset a configuration option");

      GNAT.Command_Line.Define_Switch
        (Config      => Config,
         Output      => Cmd.Global'Access,
         Long_Switch => "--global",
         Help        => "Set and Unset global configuration instead" &
                         " of the local one");

      GNAT.Command_Line.Define_Switch
        (Config      => Config,
         Output      => Cmd.Builtins_Doc'Access,
         Long_Switch => "--builtins-doc",
         Help        =>
           "Print Markdown documention of the built-in configuration options");

   end Setup_Switches;

end Alr.Commands.Config;
