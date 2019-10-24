with Alire.VCSs.SVN;

package body Alire.Origins.Deployers.SVN is

   ------------
   -- Deploy --
   ------------

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome is
   begin
      return VCSs.SVN.Handler.Clone (This.Base.URL_With_Commit, Folder);
   end Deploy;

   -----------
   -- Fetch --
   -----------

   overriding
   function Fetch (This   : Deployer; Folder : String) return Outcome is
     (Outcome_Success);

end Alire.Origins.Deployers.SVN;
