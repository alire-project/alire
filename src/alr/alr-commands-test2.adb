with Alire.Directories;
with Alr.Test_Runner;

package body Alr.Commands.Test2 is
   overriding procedure Execute
     (Cmd : in out Command; Args : AAA.Strings.Vector)
   is
      use Alire.Directories;
      use GNAT.Strings;

      Subfolder : constant String :=
        (if Cmd.Directory.all /= "" then Cmd.Directory.all else "tests");
   begin
      Cmd.Requires_Workspace;
      Cmd.Set (Alire.Roots.Load_Root (Cmd.Root.Path / Subfolder));
      Cmd.Requires_Workspace (Sync => True);

      declare
         G : Guard (Enter_Folder (Cmd.Root.Path));
         pragma Unreferenced (G);
      begin
         Alr.Test_Runner.Run
           (Cmd.Root, AAA.Strings.Empty_Vector, Integer'Max (Cmd.Jobs, 0));
      end;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding function Long_Description
     (Cmd : Command) return AAA.Strings.Vector is
     (AAA.Strings.Empty_Vector.Append
        ("Run tests in a predefined format using the (experimental) " &
         "default alire test runner"));

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
   begin
      Define_Switch
        (Config, Cmd.Jobs'Access, "-j:", "--jobs=",
         "Run up to N tests in parallel, or as many as processors " &
         "if 0 (default)", Default => 0, Argument => "N");

      Define_Switch
        (Config, Cmd.Directory'Access, Long_Switch => "--dir=",
         Help => "Run tests from the given folder (default: tests)",
         Argument => "<dir>");
   end Setup_Switches;
end Alr.Commands.Test2;
