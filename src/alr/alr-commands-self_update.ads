package Alr.Commands.Self_Update is
   use type AAA.Strings.Vector;

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("self-update");

   overriding
   procedure Execute (Cmd : in out Command; Args : AAA.Strings.Vector);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Self-update the alire binary");

   overriding
   function Long_Description (Cmd : Command) return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
       & "Update the Alire binary to the latest version, or to a version "
       & "specified on the command line.");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("");

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

private

   Switch_Location : constant String := "location";
   Switch_Nightly  : constant String := "nightly";
   Switch_Release  : constant String := "release";

   type Command is new Commands.Command with record
      Nightly  : aliased Boolean := False;
      Location : aliased GNAT_String;
      Release  : aliased GNAT_String;
   end record;

end Alr.Commands.Self_Update;
