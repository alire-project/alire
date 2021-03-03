with Alire.Origins;
with Alire.Publish;
with Alire.URI;

package body Alr.Commands.Publish is

   package URI renames Alire.URI;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd : in out Command) is

      function Revision return String
      is (if Num_Arguments >= 2 then Argument (2) else "");

      Options : constant Alire.Publish.All_Options :=
                  Alire.Publish.New_Options
                    (Manifest   =>
                       (if Cmd.Manifest.all /= ""
                        then Cmd.Manifest.all
                        else Alire.Roots.Crate_File_Name),
                     Skip_Build => Cmd.Skip_Build);

   begin
      if Alire.Utils.Count_True ((Cmd.Tar, Cmd.Print_Trusted)) > 1 or else
        (Cmd.Manifest.all /= "" and then Cmd.Print_Trusted)
      then
         Reportaise_Wrong_Arguments
           ("Given switches cannot be simultaneously set");
      end if;

      if Cmd.Print_Trusted then
         Alire.Publish.Print_Trusted_Sites;

      elsif Cmd.Tar then

         if Num_Arguments > 2 then
            Reportaise_Wrong_Arguments
              ("Unknown extra arguments, only a mandatory URL"
               & " and optional revision are expected");
         end if;

         Alire.Publish.Directory_Tar
           (Path     => (if Num_Arguments >= 1 then Argument (1) else "."),
            Revision => (if Num_Arguments >= 2 then Argument (2) else "HEAD"),
            Options  => Options);

      else
         if Num_Arguments < 1 then
            Alire.Publish.Local_Repository (Options => Options);

         elsif Num_Arguments > 2 then
            Reportaise_Wrong_Arguments
              ("Unknown extra arguments, only a mandatory URL"
               & " and optional revision are expected");

         else

            --  Choose between local path or remote

            declare
               use Alire.Origins;
               URL : constant String := Argument (1);
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
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin
      Define_Switch
        (Config,
         Cmd.Manifest'Access,
         "", "--manifest=",
         "Selects a manifest file other than ./alire.toml");

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

      Define_Switch
        (Config,
         Cmd.Skip_Build'Access,
         "", "--skip-build",
         "Skip the build check step");
   end Setup_Switches;

end Alr.Commands.Publish;
