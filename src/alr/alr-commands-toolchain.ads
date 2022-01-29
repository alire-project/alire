with AAA.Strings;

private with GNAT.Strings;

package Alr.Commands.Toolchain is

   --  Installation of binary toolchain crates into the ${ALR_CONFIG}/cache
   --  shared configuration.

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("toolchain");

   overriding
   procedure Execute (Cmd : in out Command;
                      Args :        AAA.Strings.Vector);

   overriding
   function Long_Description (Cmd  : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
       .Append
         ("Download toolchain elements, like" & " " & TTY.Emph ("GNAT")
          & " and " & TTY.Emph ("gprbuild") & ", in the shared cache of the"
          & " active configuration.")
       .New_Line
       .Append
         ("Run it without arguments to get a list of downloaded tools.")
       .New_Line
       .Append
         ("Use --select without arguments to run the assistant to "
          & "select the default toolchain for this configuration. "
          & "Adding --local will instead make the selection apply "
          & "only to the workspace (overriding a possible "
          & "configuration-wide selection). Giving one or more releases"
          & " argument will skip the assistant and set the release as the"
          & " default.")
       .New_Line
       .Append
         ("Specify --install/--uninstall and one or more crates name with"
          & " optional version set to make available or remove a tool.")
       .New_Line
       .Append
         ("Run `" & TTY.Terminal ("alr help toolchains") & "` for further "
          & "information about toolchain management and use.")
      );

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Manage Alire-provided toolchains");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[-u|--uninstall] [-i|--install crate[version set]] |"
       & " --select [--local] [releases] [--disable-assistant]");

private

   type Command is new Commands.Command with record
      Disable     : aliased Boolean := False;
      Install     : aliased Boolean := False;
      Install_Dir : aliased GNAT.Strings.String_Access := null;
      Local       : aliased Boolean := False;
      S_Select    : aliased Boolean := False;
      Uninstall   : aliased Boolean := False;
   end record;

end Alr.Commands.Toolchain;
