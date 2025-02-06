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
                              return AAA.Strings.Vector;

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Help publishing a new version of a crate");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[--skip-build] [--skip-submit|--for-private-index] [--tar] "
       & "[--manifest <file>] [<URL> [commit]]] [--request-review NUM]");

private

   type Command is new Commands.Command with record
      Manifest : aliased GNAT.Strings.String_Access;
      --  A manifest to use instead of the default one.

      Print_Trusted : aliased Boolean := False;

      Skip_Build : aliased Boolean := False;
      --  Skip the build check

      Skip_Submit : aliased Boolean := False;
      --  Skip checking user's GitHub account, and stop after manifest
      --  generation instead of asking the user to continue

      For_Private_Index : aliased Boolean := False;
      --  Skip_Submit, and also disable checks which only apply to the
      --  community index

      Cancel     : aliased GNAT.Strings.String_Access := new String'(Unset);
      --  Number of a PR to prematurely close

      Reason     : aliased GNAT.Strings.String_Access := new String'(Unset);
      --  Reason to give when closing the PR

      Review     : aliased GNAT.Strings.String_Access := new String'(Unset);
      --  True when requesting a review for a PR

      Status     : aliased Boolean := False;
      --  Retrieve the status of PRs opened by the user

      Tar        : aliased Boolean := False;
      --  Start the assistant from a local folder to be tar'ed and uploaded
   end record;

end Alr.Commands.Publish;
