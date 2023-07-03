with Alire.Directories;
with Alire.Errors;
with Alire.GitHub;
with Alire.Index;
with Alire.OS_Lib;
with Alire.Paths;
with Alire.Platforms.Folders;
with Alire.Publish.States;
with Alire.TOML_Index;
with Alire.URI;
with Alire.Utils.TTY;
with Alire.Utils.User_Input.Query_Config;
with Alire.VCSs.Git;
with Alire.Version;
with Alire.VFS;

with CLIC.User_Input;

package body Alire.Publish.Submit is

   package User_Info renames Utils.User_Input.Query_Config;

   use URI.Operators;

   subtype Vector is AAA.Strings.Vector;

   --  Remote names used through
   Origin        : constant String := "origin";
   Upstream      : constant String := "upstream";
   Upstream_Base : constant String := Upstream / Index.Community_Branch;
   Upstream_Repo : constant URL    := Index.Community_Host
                                      / Index.Community_Organization
                                      / Index.Community_Repo_Name;

   ----------------
   -- Remote_URL --
   ----------------

   function User_Remote_URL return String
   --  Must be a function so the user is not prematurely queried for the login
   is (Index.Community_Host
       / User_Info.User_GitHub_Login
       / Index.Community_Repo_Name);

   ---------------------
   -- Local_Repo_Path --
   ---------------------

   function Local_Repo_Path return Absolute_Path
   is (Platforms.Folders.Cache / "publish" / "community");

   -------------------
   -- Ask_For_Token --
   -------------------

   function Ask_For_Token (Reason : String) return String is

      --------------
      -- Validate --
      --------------

      function Validate (S : String) return Boolean
      is (S /= "");

      GH_Token : constant String := OS_Lib.Getenv (GitHub.Env_GH_Token, "");
   begin
      if GH_Token = "" then
         Put_Info
           (TTY.Terminal ("alr") & " requires a GitHub Personal Access "
            & "Token (PAT) " & Reason & ". To avoid being asked for it "
            & "every time, you can define the GH_TOKEN environment "
            & "variable.");
         Put_Info
           ("You can create access tokens at " & TTY.URL (GitHub.URL_Tokens));
      end if;

      return (if GH_Token /= ""
              then GH_Token
              else
                 CLIC.User_Input.Query_String
                ("Please provide your GitHub Personal Access Token: ",
                 Default    => "",
                 Validation => Validate'Unrestricted_Access));
   end Ask_For_Token;

   ------------
   -- Exists --
   ------------

   procedure Exists (Context : in out Data) is
   begin
      --  Detect root we are going to need

      Context.Root := Roots.Optional.Search_Root (Directories.Current);
      if not Context.Root.Is_Valid then
         Raise_Checked_Error ("Cannot continue outside of a workspace");
      end if;

      declare
         Busy : constant Trace.Ongoing :=
                  Trace.Activity ("Looking up pull requests")
                  with Unreferenced;

         Status : constant States.PR_Status
           := States.Find_Pull_Request (Context.Root.Value.Release.Milestone);
      begin
         if Status.Is_Open then
            Raise_Checked_Error
              (Errors.New_Wrapper
               .Wrap ("There is already an open pull request for "
                 & Utils.TTY.Name (Context.Root.Value.Name))
               .Wrap ("Visit " & TTY.URL (Status.Webpage) & " for details")
               .Get);
         end if;
      end;

      Put_Success ("No conflicting pull request found");
   end Exists;

   ---------------
   -- Fork_Repo --
   ---------------

   procedure Fork (Context : in out Data) is
      use all type CLIC.User_Input.Answer_Kind;
      use all type GitHub.Async_Result;
   begin

      --  Verify manifest to publish was generated at expected place

      if not Directories.Is_File (Context.Generated_Manifest) then
         Raise_Checked_Error ("Cannot continue: manifest missing at "
                              & TTY.URL (Context.Generated_Manifest));
      end if;

      Context.Token := +Ask_For_Token
        ("to fork the community index to your account");

      if GitHub.Repo_Exists then
         Put_Success ("Community index fork exists in user account");
         return;
      else
         if CLIC.User_Input.Query
           ("A fork of the community index will now be created into your "
            & "GitHub account to be able to submit a pull request. "
            & "Do you agree?",
            Valid   => (Yes | No => True, others => False),
            Default => Yes) /= Yes
         then
            Raise_Checked_Error
              ("Cannot continue with automatic submission");
         end if;
      end if;

      case GitHub.Fork (Owner => Index.Community_Organization,
                        Repo  => Index.Community_Repo_Name,
                        Token => +Context.Token)
      is
         when Pending =>
            Raise_Checked_Error
              ("Forking is still ongoing, "
               & "please try again in a few minutes");
         when Completed =>
            Put_Success ("Fork of community index completed");
      end case;
   end Fork;

   -----------
   -- Clone --
   -----------

   procedure Clone (Context : in out Data) is

      use all type Vector;

      ----------------------
      -- Prepare_Upstream --
      ----------------------

      procedure Prepare_Upstream is
      begin
         --  Ensure upstream is proper by removing/readding it
         if VCSs.Git.Remotes (Local_Repo_Path).Contains (Upstream) then
            VCSs.Git.Command
              (Local_Repo_Path,
               To_Vector ("remote") & "remove" & Upstream).Discard_Output;
         end if;

         --  Set up the upstream remote
         VCSs.Git.Add_Remote (Local_Repo_Path,
                              Name => Upstream,
                              URL  => Upstream_Repo);
      end Prepare_Upstream;

      ----------
      -- Pull --
      ----------

      procedure Pull is
      begin
         --  Start by preparing the base upstream remote

         Prepare_Upstream;

         --  Fetch any upstream remote changes

         VCSs.Git.Command (Local_Repo_Path,
                           To_Vector ("fetch") & Upstream).Discard_Output;

         --  Force-checkout the branch we want

         VCSs.Git.Command (Local_Repo_Path,
                           To_Vector ("checkout")
                           & Upstream_Base
                           & "-B" & Context.Branch_Name).Discard_Output;

         --  And ensure the situation is pristine to add our new manifest

         VCSs.Git.Command (Repo => Local_Repo_Path,
                           Args =>
                             To_Vector ("reset")
                           & "--hard"
                           & Upstream_Base).Discard_Output;
         --  Discard any local changes

         VCSs.Git.Command (Repo => Local_Repo_Path,
                           Args => To_Vector ("clean") & "-fd").Discard_Output;
         --  Drop any spurious files/folders (like other submitted manifests)

         Put_Success ("Local index updated successfully");
      end Pull;

   begin
      if Directories.Is_Directory (Local_Repo_Path)
        and then VCSs.Git.Handler.Is_Repository (Local_Repo_Path)
        and then VCSs.Git.Handler.Remote_URL (Local_Repo_Path) =
                 User_Remote_URL
      then
         --  It's enough to refresh the local repo
         Pull;
         return;
      end if;

      Directories.Force_Delete (Local_Repo_Path);
      --  Delete a possibly outdated repo

      VCSs.Git.Handler.Clone
        (From   => Index.Community_Host
         / User_Info.User_GitHub_Login
         / Index.Community_Repo_Name,
         Into   => Local_Repo_Path).Assert;

      --  We can reuse the pull logic now to set up the local branch
      Pull;
      Put_Success ("Community index cloned succesfully");
   end Clone;

   ----------
   -- Push --
   ----------

   procedure Push (Context : in out Data) is

      Filename : constant String :=
                   TOML_Index.Manifest_File
                     (Context.Root.Value.Name,
                      Context.Root.Value.Release.Version);

      Manifest : constant Absolute_Path :=
                   Context.Root.Value.Working_Folder
                   / Paths.Release_Folder_Inside_Working_Folder
                   / Filename;

      ----------
      -- Copy --
      ----------

      procedure Copy is
         Target : constant Absolute_Path
           := Local_Repo_Path
              / VFS.To_Native
                 (TOML_Index.Manifest_Path (Context.Root.Value.Name))
              / Filename;
      begin
         Directories.Create_Tree (Directories.Parent (Target));
         Directories.Adirs.Copy_File (Manifest, Target);
         Put_Success ("Manifest copied into place: " & TTY.URL (Target),
                      Trace.Detail);
      end Copy;

      ------------
      -- Commit --
      ------------

      procedure Commit is
         use all type AAA.Strings.Vector;
         use all type VCSs.Git.States;
      begin
         --  Add files first, so if there's something new it's detected as
         --  dirty.

         VCSs.Git.Command (Local_Repo_Path,
                           To_Vector ("add") & ".").Discard_Output;

         --  Now we can check if there's something to commit or not

         if VCSs.Git.Handler.Status (Local_Repo_Path) = Dirty then
            VCSs.Git.Commit_All
              (Local_Repo_Path,
               Msg => Context.PR_Name
                      & " (via `alr publish --submit`)").Assert;
         else
            Put_Warning
              ("Nothing to commit: "
               & "manifest was already in repository");
         end if;
      end Commit;

      ------------
      -- Upload --
      ------------

      procedure Upload is
      begin
         VCSs.Git.Push (Local_Repo_Path,
                        Remote => Origin,
                        Token  => +Context.Token,
                        Force  => True,
                        Create => True).Assert;
         Put_Success ("Manifest pushed into remote index");
      end Upload;

   begin
      Copy;
      Commit;
      Upload;
   end Push;

   ------------------
   -- Request_Pull --
   ------------------

   procedure Request_Pull (Context : in out Data) is
      use all type CLIC.User_Input.Answer_Kind;
   begin
      if CLIC.User_Input.Query
        ("A pull request is about to be created on "
         & TTY.URL (Upstream_Repo / "pulls")
         & New_Line
         & "Do you want to continue?",
         Valid   => (No | Yes => True, others => False),
         Default => Yes) /= Yes
      then
         Raise_Checked_Error ("Cancelled");
      end if;

      declare
         Busy : constant Trace.Ongoing :=
                  Trace.Activity ("Opening pull request")
                  with Unreferenced;

         Number : constant Natural
           := GitHub.Create_Pull_Request
             (Draft       => True,
              Token       => +Context.Token,
              Head_Branch => Context.Branch_Name,
              Title       => Context.PR_Name,
              Message     =>
                "Created via `alr publish` with `alr "
                & Version.Current & "`");
      begin
         Put_Success ("Pull request created successfully");
         Put_Info ("Visit " & TTY.URL (States.Webpage (Number))
                   & " for details");
         Put_Warning ("The submission is in " & TTY.Emph ("draft mode") & ". "
                      & "Please visit the given URL and request a review once "
                      & "automated checks have succeeded.",
                      Trace.Info);
      end;
   end Request_Pull;

end Alire.Publish.Submit;
