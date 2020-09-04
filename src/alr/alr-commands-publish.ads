with GNAT.Strings;

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
   is ("--hash <origin> | " & Switch_Prepare & " <URL> [--commit <id>]");

private

   type Command is new Commands.Command with record
      Commit  : aliased GNAT.Strings.String_Access;
      --  The commit of a remote repository

      Hash    : aliased Boolean := False;
      --  Compute hash of given origin

      Prepare : aliased Boolean := False;
      --  Start the assistant with ready sources and manifest; only verify and
      --  add the origin.
   end record;

end Alr.Commands.Publish;
