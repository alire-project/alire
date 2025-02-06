with Alire.Formatting;
with Alire.Origins;
with Alire.Publish.States;
with Alire.URI;
with Alire.Utils;

package body Alr.Commands.Publish is

   package Format renames Alire.Formatting;
   package URI renames Alire.URI;

   ------------
   -- To_Int --
   ------------

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
                    (Manifest          =>
                       (if Cmd.Manifest.all /= ""
                        then Cmd.Manifest.all
                        else Alire.Roots.Crate_File_Name),
                     Skip_Build        => Cmd.Skip_Build,
                     Skip_Submit       =>
                       --  "--for-private-index" implies "--skip-submit"
                       Cmd.Skip_Submit or else Cmd.For_Private_Index,
                     For_Private_Index => Cmd.For_Private_Index);

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
         Alire.Publish.Print_Trusted_Sites
           (For_Community => not Cmd.For_Private_Index);

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
               if URI.URI_Kind (URL) in URI.Bare_Path then
                  if Archive_Format (URL) /= Unknown then
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

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
       .Append ("Checks a release and generates an index manifest.")
       .New_Line
       .Append ("See full details at")
       .New_Line
       .Append
         (" https://github.com/alire-project/alire/blob"
          & Format.Format ("/${ALIRE_VERSION}/", Format.For_Github_URL,
                           Convert_Path_Seps => False)
          & "doc/publishing.md")
       .New_Line
       .Append ("URL is an optional path to a remote source archive, or"
         & " a local or remote git repository.")
       .New_Line
       .Append ("For the common use case of a github-hosted repository,"
         & " issue `alr publish` after committing and pushing"
         & " the new release version.")
       .New_Line
       .Append ("See the above link for instructions on how to create a "
         & "Github Personal Access Token (PAT), needed to allow `alr` to "
         & "interact with Github (forking, PR creation) on your behalf.")
       .New_Line
       .Append ("Use --tar to create a source archive ready to be uploaded.")
       .New_Line
       .Append ("Use --manifest to use metadata in a non-default file.")
       .New_Line
       .Append ("See the above link for help with other scenarios."));

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
         "Selects a manifest file other than ./alire.toml",
         Argument => "FILE");

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
         Cmd.For_Private_Index'Access,
         "", "--for-private-index",
         "The same as --skip-submit, but additionally disable checks which "
         & "are specific to the community index and may not apply to others");

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
