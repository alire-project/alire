with Alire.Directories;
with Alire.GitHub;
with Alire.Paths;
with Alire.Platforms.Folders;
with Alire.TOML_Index;
with Alire.VCSs.Git;

with CLIC.User_Input;

package body Alire.Publish.Automate is

   use Directories.Operators;

   ----------
   -- Repo --
   ----------

   function Repo return Absolute_Path
   is (Platforms.Folders.Cache / "publish" / "community");

   ---------------
   -- Create_PR --
   ---------------

   procedure Create_PR (Root : Roots.Root) is

      Branch : constant String := "alr_publish_submit";

      ---------------
      -- Fork_Repo --
      ---------------

      procedure Fork_Repo is
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

         case GitHub.Fork (Owner => "alire-project",
                           Repo  => "alire-index")
         is
            when Pending =>
               Raise_Checked_Error
                 ("Forking is still ongoing, "
                  & "please try again in a few minutes");
            when Completed =>
               Put_Success ("Fork of community index completed");
         end case;
      end Fork_Repo;

      -------------------------------
      -- Prepare_Local_Index_Clone --
      -------------------------------

      procedure Prepare_Local_Index_Clone is
         use all type AAA.Strings.Vector;
      begin
         if Directories.Is_Directory (Repo)
           and then VCSs.Git.Handler.Is_Repository (Repo)
         then
            --  Simplest case, we already had cloned the repo before

            --  Refresh the repo to ensure it is pristine. To do so, we
            --  force-create the branch for submission and reset its contents

            VCSs.Git.Handler.Update (Repo, Branch => Branch).Assert;

            VCSs.Git.Command (Repo, To_Vector ("reset") & "--hard").Ignore;

         else
            --  We need to clone the repo to the user account if it doesn't
            --  already have a fork

            Fork_Repo;
            Prepare_Local_Index_Clone;
            return;

         end if;
      end Prepare_Local_Index_Clone;

      Manifest : constant Absolute_Path :=
                   Root.Working_Folder
                     / Paths.Release_Folder_Inside_Working_Folder
                     / TOML_Index.Manifest_File (Root.Name,
                                                 Root.Release.Version);
   begin
      if not Directories.Is_File (Manifest) then
         Raise_Checked_Error ("Cannot continue: manifest missing at "
                              & TTY.URL (Manifest));
      end if;

      --  Now it's a matter of forking the repo, copying the file, committing
      --  and pushing changes, and creating the PR...

      Prepare_Local_Index_Clone;
   end Create_PR;

end Alire.Publish.Automate;
