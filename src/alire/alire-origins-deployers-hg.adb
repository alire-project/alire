with Alire.VCSs.Hg;

package body Alire.Origins.Deployers.Hg is

   ------------
   -- Deploy --
   ------------

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome is
   begin
      return VCSs.Hg.Handler.Clone (This.Base.URL_With_Commit, Folder);
   end Deploy;

   -----------
   -- Fetch --
   -----------

   overriding
   function Fetch (This   : Deployer; Folder : String) return Outcome is
     (Outcome_Success);

end Alire.Origins.Deployers.Hg;
