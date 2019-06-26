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

end Alire.Origins.Deployers.Hg;
