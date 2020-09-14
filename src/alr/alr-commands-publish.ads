package Alr.Commands.Publish is

   --  Publish lends a helping hand to automate submission of crates/releases.

   type Command is new Commands.Command with private;

   overriding
   procedure Execute (Cmd : in out Command);

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector
   is (Alire.Utils.Empty_Vector
       .Append ("Checks a release and generates an index manifest")
       .New_Line
       .Append ("See full details at")
       .New_Line
       .Append (" https://github.com/alire-project/alire/blob/master/"
                & "doc/publishing.md")
       .New_Line
       .Append ("URL is an optional path to a remote source archive, or"
                & " a local or remote git repository.")
       .New_Line
       .Append ("For the common use case of a github-hosted repository,"
                & " issue `alr publish` after committing and pushing"
                & " the new release version.")
       .New_Line
       .Append ("See the above link for help with other scenarios."));

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Help with the publication of a new release");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[--skip-build] [<URL> [commit]]");

private

   type Command is new Commands.Command with record
      Print_Trusted : aliased Boolean := False;

      Skip_Build : aliased Boolean := False;
      --  Skip the build check
   end record;

end Alr.Commands.Publish;
