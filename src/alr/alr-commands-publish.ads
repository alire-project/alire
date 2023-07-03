with AAA.Strings;

private with GNAT.Strings;

package Alr.Commands.Publish is

   --  Publish lends a helping hand to automate submission of crates/releases.

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("publish");

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
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
       .Append ("Use --tar to create a source archive ready to be uploaded.")
       .New_Line
       .Append ("Use --manifest to use metadata in a non-default file.")
       .New_Line
       .Append ("See the above link for help with other scenarios."));

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Help publishing a new version of a crate");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[--skip-build] [--skip-submit] [--tar] "
       & "[--manifest <file>] [<URL> [commit]]]");

private

   type Command is new Commands.Command with record
      Manifest : aliased GNAT.Strings.String_Access;
      --  A manifest to use instead of the default one.

      Print_Trusted : aliased Boolean := False;

      Skip_Build : aliased Boolean := False;
      --  Skip the build check

      Skip_Submit : aliased Boolean := False;
      --  Stop after generation instead of asking the user to continue

      Tar        : aliased Boolean := False;
      --  Start the assistant from a local folder to be tar'ed and uploaded
   end record;

end Alr.Commands.Publish;
