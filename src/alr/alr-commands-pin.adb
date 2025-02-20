with Alire.Dependencies;
with Alire.Optional;
with Alire.Roots.Editable;
with Alire.Solutions;
with Alire.URI;
with Alire.Utils.User_Input;
with Alire.Utils.TTY;

with Alr.Commands.User_Input;

with Semantic_Versioning.Extended;

with TOML_Slicer;

package body Alr.Commands.Pin is

   package Semver renames Semantic_Versioning;

   --------------------
   -- Change_One_Pin --
   --------------------

   procedure Change_One_Pin (Cmd    : in out Command;
                             Root   : in out Alire.Roots.Editable.Root;
                             Target :        String)
   is
      Version  : Semver.Version;
      Solution : constant Alire.Solutions.Solution := Root.Solution;
      Dep      : constant Alire.Dependencies.Dependency :=
                   Alire.Dependencies.From_String (Target);
      --  Only crate=version should be allowed here, we check it shortly

      ---------
      -- Pin --
      ---------

      procedure Pin is
      begin

         --  We let to re-pin without checks because the requested version may
         --  be different.

         Root.Add_Version_Pin (Dep.Crate, Version);

      end Pin;

      -----------
      -- Unpin --
      -----------

      procedure Unpin is
      begin
         if not Solution.State (Dep.Crate).Is_User_Pinned then
            Reportaise_Command_Failed ("Requested crate is already unpinned");
         end if;

         Root.Remove_Pin (Dep.Crate);
      end Unpin;

   begin

      --  Sanity checks

      if not Solution.Depends_On (Dep.Crate) then
         Reportaise_Command_Failed
           ("Cannot " & (if Cmd.Unpin then "unpin" else "pin")
            & " dependency not in solution: "
            & Alire.Utils.TTY.Name (Dep.Crate));
      end if;

      --  Check if we are given a particular version

      if AAA.Strings.Contains (Target, "=") then

         if Cmd.Unpin then
            Reportaise_Wrong_Arguments ("Unpinning does not require version");
         end if;

         Version := Semver.Parse (AAA.Strings.Tail (Dep.Image, '='),
                                  Relaxed => False);

         Trace.Debug ("Pin requested for exact version: "
                      & Version.Image);

      elsif Solution.State (Dep.Crate).Is_Solved then
         Version := Solution.State (Dep.Crate).Release.Version;
      elsif not Cmd.Unpin then
         Reportaise_Wrong_Arguments
           ("An explicit version is required to pin a crate with"
            & " no release in the current solution: "
            & Alire.Utils.TTY.Name (Dep.Crate));
      end if;

      --  Proceed to pin/unpin

      if Cmd.Unpin then
         Unpin;
      else
         Pin;
      end if;
   end Change_One_Pin;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is

      -------------------------
      -- Validate_Crate_Spec --
      -------------------------

      procedure Validate_Crate_Spec (Spec : String) is
         Dep : constant Alire.Dependencies.Dependency :=
                 Alire.Dependencies.From_String (Spec);
      begin
         if not Dep.Versions.Is_Any then
            if not AAA.Strings.Has_Prefix (Dep.Versions.Image, "=") then
               Reportaise_Wrong_Arguments
                 ("Plain crate name or crate=version argument expected for"
                  & " pinning, but got: " & TTY.Emph (Spec));
            end if;
         end if;
      end Validate_Crate_Spec;

   begin

      --  Argument validation

      if Cmd.Pin_All and then Args.Count /= 0 then
         Reportaise_Wrong_Arguments ("--all must appear alone");
      elsif Cmd.URL.all /= "" and then
        (Args.Count /= 1 or else Cmd.Pin_All or else Cmd.Unpin)
      then
         Reportaise_Wrong_Arguments
           ("--use must be used alone with a crate name");
      elsif Cmd.Commit.all /= "" and then Cmd.Branch.all /= "" then
         Reportaise_Wrong_Arguments
           ("Cannot specify both a branch and a commit simultaneously");
      end if;

      Cmd.Requires_Workspace;

      --  Listing of pins

      if not Cmd.Pin_All and then Args.Count = 0 then
         Cmd.Root.Solution.Print_Pins;
         return;
      elsif Args.Count > 1 then
         Reportaise_Wrong_Arguments
           ("Pin expects a single crate or crate=version argument");
      elsif Args.Count = 1 then
         --  Check that we get either a plain name or a crate=version
         Validate_Crate_Spec (Args (1));
      end if;

      --  Apply changes;

      declare
         New_Root : Alire.Roots.Editable.Root :=
                      Alire.Roots.Editable.New_Root (Original => Cmd.Root);
         Optional_Crate : constant Alire.Optional.Crate_Name :=
                            (if Args.Count = 1
                             then Alire.Optional.Crate_Names.Unit
                               (Alire.Dependencies
                                     .From_String (Args (1)).Crate)
                             else Alire.Optional.Crate_Names.Empty);
      begin

         if Cmd.Pin_All then

            --  Change all pins

            for Crate of Cmd.Root.Solution.Crates loop
               Change_One_Pin (Cmd, New_Root, Crate.As_String);
            end loop;

         elsif Cmd.URL.all /= "" then

            if Cmd.Commit.all /= "" or else Cmd.Branch.all /= ""
              or else Alire.URI.URI_Kind (Cmd.URL.all) in Alire.URI.Git_URIs
            then

               --  Pin to remote commit

               New_Root.Add_Remote_Pin
                 (Crate  => Optional_Crate,
                  Origin => Cmd.URL.all,
                  Ref    => Cmd.Commit.all,
                  Branch => Cmd.Branch.all,
                  Subdir => Cmd.Subdir.all);

            else

               --  Ensure no subdir for local pins

               if Cmd.Subdir.all /= "" then
                  Reportaise_Wrong_Arguments
                    ("Pins to local directories do not accept the "
                     & TTY.Terminal ("--subdir") & " switch");
               end if;

               --  Pin to dir, with a warning if it doesn't look like a path
               --  and a subsequent confirmation prompt if it doesn't exist.

               declare
                  use Alire.URI;
                  Local : constant Boolean :=
                    URI_Kind (Cmd.URL.all) in Local_URIs;
                  Path  : constant String :=
                    (if Local then Local_Path (Cmd.URL.all) else Cmd.URL.all);
               begin
                  if not Local then
                     Alire.Put_Warning
                       ("Assuming '" & Cmd.URL.all & "' is a directory "
                        & "because no branch or commit was specified.");
                  end if;

                  if not Alire.Utils.User_Input.Approve_Dir (Path) then
                     Trace.Info ("Abandoned by user.");
                     return;
                  end if;

                  New_Root.Add_Path_Pin (Crate => Optional_Crate,
                                         Path  => Path);
               end;

            end if;

            --  Report crate detection at target destination

            User_Input.Report_Pinned_Crate_Detection (Optional_Crate.Element,
                                                      New_Root.Solution);

         else

            --  Change a single pin

            Change_One_Pin (Cmd, New_Root, Args (1));
         end if;

         --  Consolidate changes

         New_Root.Confirm_And_Commit;
      end;

   exception
      when E : Semver.Malformed_Input =>
         Alire.Log_Exception (E);
         Reportaise_Wrong_Arguments ("Improper version string");
      when E : TOML_Slicer.Slicing_Error =>
         Alire.Log_Exception (E);
         Reportaise_Command_Failed
           ("alr was unable to apply your request; "
            & "please edit the manifest manually.");
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector is
     (AAA.Strings.Empty_Vector
      .Append ("Pin releases to a particular version."
               & " By default, the current solution version is used."
               & " A pinned release is not affected by automatic updates.")
      .New_Line
      .Append ("Without arguments, show existing pins.")
      .New_Line
      .Append ("Use --all to pin the whole current solution.")
      .New_Line
      .Append ("Specify a single crate to modify its pin.")
      .New_Line
      .Append ("Use the --use <PATH|URL> switch to"
               & " use the target to fulfill a dependency locally"
               & " instead of looking for indexed releases."
               & " An optional reference can be specified with --commit;"
               & " the pin will be frozen at the commit currently matching"
               & " the reference.  Alternatively, a branch to track can be"
               & " specified with --branch. Use `alr update` to refresh the"
               & " tracking pin contents.")
     );

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
      Define_Switch (Config,
                     Cmd.Pin_All'Access,
                     Long_Switch => "--all",
                     Help        => "Pin the complete solution");

      Define_Switch (Config,
                     Cmd.Unpin'Access,
                     Long_Switch => "--unpin",
                     Help        => "Unpin a release");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Branch'Access,
         Long_Switch => "--branch=",
         Argument    => "NAME",
         Help        => "Branch to be tracked in repository");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Commit'Access,
         Long_Switch => "--commit=",
         Argument    => "REF",
         Help        => "Reference to be retrieved from repository");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Subdir'Access,
         Long_Switch => "--subdir=",
         Argument    => "REL_PATH",
         Help        => "Relative path to crate inside repository");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.URL'Access,
         Long_Switch => "--use=",
         Argument    => "PATH|URL",
         Help        =>
           "Use a directory or repository to fulfill a dependency");
   end Setup_Switches;

end Alr.Commands.Pin;
