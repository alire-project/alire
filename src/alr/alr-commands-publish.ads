package Alr.Commands.Publish is

   --  Publish lends a helping hand to automate submission of crates/releases.

   Switch_Prepare : constant String := "--prepare";

   type Command is new Commands.Command with private;

   overriding
   procedure Execute (Cmd : in out Command);

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector
   is (Alire.Utils.Empty_Vector
       .Append ("Work in progress, manual instructions available at:")
       .New_Line
       .Append ("   https://github.com/alire-project/alire/blob/master/"
                & "doc/publishing.md")
       .New_Line
       .Append ("<origin> is a tarball URL or local path."));

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Helps with the publication of new crates and releases");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is (Switch_Prepare & " [<URL> [commit]]");

private

   type Command is new Commands.Command with record
      Prepare : aliased Boolean := False;
      --  Start the assistant with ready sources and manifest; only verify and
      --  add the origin.

      Print_Trusted : aliased Boolean := False;

      Skip_Build : aliased Boolean := False;
      --  Skip the build check
   end record;

end Alr.Commands.Publish;
