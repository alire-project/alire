with Alire.TTY;

package Alr.Commands.Install is

   package TTY renames Alire.TTY;

   --  Installation of binary releases and shared dependencies into the
   --  ${ALR_CONFIG}/cache shared configuration.

   type Command is new Commands.Command with private;

   overriding
   procedure Execute (Cmd : in out Command);

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector
   is (Alire.Utils.Empty_Vector
       .Append
         ("Install toolchain elements, like" & " " & TTY.Emph ("GNAT")
          & " and " & TTY.Emph ("gprbuid") & ", in the shared cache of the"
          & " active configuration.")
       .New_Line
       .Append
         ("Run it without arguments to get a list of installed releases.")
       .New_Line
       .Append
         ("Specify a crate name with optional version set to request the"
          & " crate installation.")
      );

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Manage Alire-provided toolchains and shared dependencies");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[-u|--uninstall] [crate[version set]] | --toolchain");

private

   type Command is new Commands.Command with record
      Toolchain : aliased Boolean := False;
      Uninstall : aliased Boolean := False;
   end record;

end Alr.Commands.Install;
