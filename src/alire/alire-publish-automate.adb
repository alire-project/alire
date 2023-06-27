with Alire.Directories;
with Alire.GitHub;
with Alire.Index;
with Alire.OS_Lib;
with Alire.Paths;
with Alire.Platforms.Folders;
with Alire.TOML_Index;
with Alire.Utils.User_Input.Query_Config;
with Alire.VCSs.Git;
with Alire.Version;
with Alire.VFS;

with CLIC.User_Input;

package body Alire.Publish.Automate is

   package User_Info renames Utils.User_Input.Query_Config;

   use Directories.Operators;

   subtype Vector is AAA.Strings.Vector;

   ----------------
   -- Remote_URL --
   ----------------

   function Remote_URL return String
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

   ---------------
   -- Create_PR --
   ---------------

   procedure Create_PR (Root : Roots.Root) is

      Token : constant String :=
                Ask_For_Token ("to fork the community index to your account");

      ---------------
      -- Fork_Repo --
      ---------------

      procedure Fork is
         use all type CLIC.User_Input.Answer_Kind;
         use all type GitHub.Async_Result;
      begin
         if GitHub.Repo_Exists then
            Put_Success ("Community index fork exists in user account");
            return;
         else
            if CLIC.User_Input.Query
              ("A fork of the community index will now be created into your "
               & "GitHub account to be able to submit a pull request. "
               & "Do you agree?",
               Valid => (Yes | No => True, others => False),
               Default => Yes) /= Yes
            then
               Raise_Checked_Error
                 ("Cannot continue with automatic submission");
            end if;
         end if;

         case GitHub.Fork (Owner => Index.Community_Organization,
                           Repo  => Index.Community_Repo_Name,
                           Token => Token)
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

      function Clone return Boolean is
         --  True when clone happened, False if not needed
      begin
         if Directories.Is_Directory (Local_Repo_Path)
           and then VCSs.Git.Handler.Is_Repository (Local_Repo_Path)
           and then VCSs.Git.Handler.Remote_URL (Local_Repo_Path) = Remote_URL
         then
            return False;
         end if;

         Directories.Force_Delete (Local_Repo_Path);
         --  Delete a possibly outdated repo

         VCSs.Git.Handler.Clone
           (From   => Index.Community_Host
                      / User_Info.User_GitHub_Login
                      / Index.Community_Repo_Name,
            Into   => Local_Repo_Path,
            Branch => Index.Community_Branch).Assert;

         Put_Success ("Community index cloned succesfully");
         return True;
      end Clone;

      ----------
      -- Pull --
      ----------

      procedure Pull is
         use all type Vector;
      begin
         VCSs.Git.Handler.Update (Local_Repo_Path,
                                  Branch => Index.Community_Branch).Assert;
         --  Fetch all remote changes

         VCSs.Git.Command (Repo => Local_Repo_Path,
                           Args =>
                             To_Vector ("reset")
                           & "--hard"
                           & (VCSs.Git.Handler.Remote (Local_Repo_Path)
                              / Index.Community_Branch)).Discard_Output;
         --  Discard any local changes

         VCSs.Git.Command (Repo => Local_Repo_Path,
                           Args => To_Vector ("clean") & "-fd").Discard_Output;
         --  Drop any spurious files/folders (like other submitted manifests)

         Put_Success ("Local index updated successfully");
      end Pull;

      Filename : constant String :=
                   TOML_Index.Manifest_File (Root.Name,
                                             Root.Release.Version);

      Manifest : constant Absolute_Path :=
                   Root.Working_Folder
                     / Paths.Release_Folder_Inside_Working_Folder
                     / Filename;

      -------------------
      -- Copy_Manifest --
      -------------------

      procedure Copy_Manifest is
         Target : constant Absolute_Path :=
                    Local_Repo_Path
                    / VFS.To_Native (TOML_Index.Manifest_Path (Root.Name))
                    / Filename;
      begin
         Directories.Create_Tree (Directories.Parent (Target));
         Directories.Adirs.Copy_File (Manifest, Target);
         Put_Success ("Manifest copied into place: " & TTY.URL (Target),
                      Trace.Detail);
      end Copy_Manifest;

      ---------------------
      -- Commit_And_Push --
      ---------------------

      procedure Commit_And_Push is
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
               Msg =>
                 Root.Name.As_String & " "
               & Root.Release.Version.Image
               & " (via `alr push`)").Assert;
         else
            Put_Warning
              ("Nothing to commit: "
               & "manifest must have been added in a previous run");
         end if;

         VCSs.Git.Push (Local_Repo_Path, Token).Assert;
         Put_Success ("Manifest pushed into remote index");
      end Commit_And_Push;

      ------------
      -- Submit --
      ------------

      procedure Submit is
      begin
         GitHub.Create_Pull_Request
           (Token   => Token,
            Title   => Root.Name.As_String & " " & Root.Release.Version.Image,
            Message =>
              "Created via `alr publish` with `alr " & Version.Current & "`");

         Put_Success ("Pull request created successfully");
      end Submit;

   begin
      if not Directories.Is_File (Manifest) then
         Raise_Checked_Error ("Cannot continue: manifest missing at "
                              & TTY.URL (Manifest));
      end if;

      --  Now it's a matter of forking the repo, copying the file, committing
      --  and pushing changes, and creating the PR... Some of these steps may
      --  not be needed and will auto-skip in that case.

      Fork;

      if not Clone then
         Pull;
      end if;

      Copy_Manifest;

      Commit_And_Push;

      Submit;
   end Create_PR;

end Alire.Publish.Automate;
