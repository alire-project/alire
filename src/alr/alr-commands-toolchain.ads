with AAA.Strings;

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
         ("Download and select toolchain elements, like" & " "
          & TTY.Emph ("GNAT") & " and " & TTY.Emph ("gprbuild")
          & ", for use by Alire.")
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
         ("Run `" & TTY.Terminal ("alr help toolchains") & "` for further "
          & "information about toolchain management and use.")
      );

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Select Alire's preferred toolchain");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("--select [--local] [releases] [--disable-assistant]");

private

   type Command is new Commands.Command with record
      Disable     : aliased Boolean := False; -- Disable assistant
      Local       : aliased Boolean := False;
      S_Select    : aliased Boolean := False;
   end record;

end Alr.Commands.Toolchain;
