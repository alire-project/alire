with Alire.Origins;
with Alire.Publish.States;
with Alire.URI;
with Alire.Utils;

package body Alr.Commands.Publish is

   package URI renames Alire.URI;

   function To_Int (S : String) return Integer is
   begin
      return Integer'Value (S);
   exception
      when others =>
         Alire.Raise_Checked_Error ("Not a valid integer: " & S);
   end To_Int;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is

      function Revision return String
      is (if Args.Count >= 2 then Args (2) else "");

      Options : constant Alire.Publish.All_Options :=
                  Alire.Publish.New_Options
                    (Manifest    =>
                       (if Cmd.Manifest.all /= ""
                        then Cmd.Manifest.all
                        else Alire.Roots.Crate_File_Name),
                     Skip_Build  => Cmd.Skip_Build,
                     Skip_Submit => Cmd.Skip_Submit);

   begin
      if Alire.Utils.Count_True
        ((Cmd.Tar, Cmd.Print_Trusted, Cmd.Status,
          Cmd.Cancel.all /= Unset, Cmd.Review.all /= Unset)) > 1
        or else
        (Cmd.Manifest.all /= "" and then Cmd.Print_Trusted)
      then
         Reportaise_Wrong_Arguments
           ("Given switches cannot be simultaneously set");
      end if;

      Cmd.Auto_Update_Index;

      if Cmd.Print_Trusted then
         Alire.Publish.Print_Trusted_Sites;

      elsif Cmd.Tar then

         if Args.Count > 2 then
            Reportaise_Wrong_Arguments
              ("Unknown extra arguments, only a mandatory URL"
               & " and optional revision are expected");
         end if;

         Alire.Publish.Directory_Tar
           (Path     => (if Args.Count >= 1 then Args (1) else "."),
            Revision => (if Args.Count >= 2 then Args (2) else "HEAD"),
            Options  => Options);

      elsif Cmd.Cancel.all /= Unset then
         if Cmd.Cancel.all = "" then
            Reportaise_Wrong_Arguments
              ("--cancel requires one pull request number");
         end if;

         if Cmd.Reason.all in "" | "unset" then
            Reportaise_Wrong_Arguments
              ("--cancel requires a --reason");
         end if;

         if not Args.Is_Empty then
            Reportaise_Wrong_Arguments
              ("Unexpected arguments; verify --reason text is quoted");
         end if;

         Alire.Publish.States.Cancel (PR     => To_Int (Cmd.Cancel.all),
                                      Reason => Cmd.Reason.all);

      elsif Cmd.Review.all /= Unset then
         if not Args.Is_Empty then
            Reportaise_Wrong_Arguments ("Unexpected arguments");
         end if;

         Alire.Publish.States.Request_Review (To_Int (Cmd.Review.all));

      elsif Cmd.Status then
         Alire.Publish.States.Print_Status;

      else
         if Args.Count < 1 then
            Alire.Publish.Local_Repository (Options => Options);

         elsif Args.Count > 2 then
            Reportaise_Wrong_Arguments
              ("Unknown extra arguments, only a mandatory URL"
               & " and optional revision are expected");

         else

            --  Choose between local path or remote

            declare
               use Alire.Origins;
               URL : constant String := Args (1);
            begin
               if URI.Scheme (URL) in URI.File_Schemes then
                  if Archive_Format (URI.Local_Path (URL)) /= Unknown then
                     --  This is a local tarball posing as a remote. Will fail
                     --  unless forced.
                     Alire.Publish.Remote_Origin (URL     => URL,
                                                  Commit  => Revision,
                                                  Options => Options);
                  else
                     --  Otherwise this is publishing based on local repo
                     Alire.Publish.Local_Repository (Path     => URL,
                                                     Revision => Revision,
                                                     Options  => Options);
                  end if;
               else
                  Alire.Publish.Remote_Origin
                    (URL     => URL,
                     Commit  => Revision, -- TODO: allow non-commits
                     Options => Options);
               end if;
            end;

         end if;
      end if;
   end Execute;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
   begin
      Define_Switch
        (Config,
         Cmd.Manifest'Access,
         "", "--manifest=",
         "Selects a manifest file other than ./alire.toml");

      Define_Switch
        (Config,
         Cmd.Skip_Build'Access,
         "", "--skip-build",
         "Skip the build check step");

      Define_Switch
        (Config,
         Cmd.Skip_Submit'Access,
         "", "--skip-submit",
         "Do not create the online pull request onto the community index");

      Define_Switch
        (Config,
         Cmd.Cancel'Access,
         "", "--cancel=",
         "Prematurely close a pull request without waiting for the merge",
         Argument => "NUM");

      Define_Switch
        (Config,
         Cmd.Reason'Access,
         "", "--reason=",
         "Give a message for the record on why the PR is being closed",
         Argument => "'short text'");

      Define_Switch
        (Config,
         Cmd.Review'Access,
         "", "--request-review=",
         "Remove draft status from the pull request and request a review",
         Argument => "NUM");

      Define_Switch
        (Config,
         Cmd.Status'Access,
         "", "--status",
         "Check the status of the last pull request for the crate");

      Define_Switch
        (Config,
         Cmd.Tar'Access,
         "", "--tar",
         "Start the publishing assistant to create a source archive"
         & " from a local directory");

      Define_Switch
        (Config,
         Cmd.Print_Trusted'Access,
         "", "--trusted-sites",
         "Print a list of trusted git repository sites");
   end Setup_Switches;

end Alr.Commands.Publish;
