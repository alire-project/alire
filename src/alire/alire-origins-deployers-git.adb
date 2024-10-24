with Alire.Settings.Builtins;
with Alire.Directories;
with Alire.VCSs.Git;

package body Alire.Origins.Deployers.Git is

   use Directories.Operators;

   ------------
   -- Deploy --
   ------------

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome is
   begin
      VCSs.Git.Handler.Clone (This.Base.URL, Folder, This.Base.Commit).Assert;

      if Settings.Builtins.Dependencies_Git_Keep_Repository.Get then

         Trace.Debug ("Keeping git repo from " & This.Base.TTY_URL_With_Commit
                      & " at " & TTY.URL (Folder));

      else

         Directories.Delete_Tree (Folder / VCSs.Git.Git_Dir);

      end if;

      return Outcome_Success;
   end Deploy;

   -----------
   -- Fetch --
   -----------

   overriding
   function Fetch (This   : Deployer; Folder : String) return Outcome is
     (Outcome_Success);

end Alire.Origins.Deployers.Git;
