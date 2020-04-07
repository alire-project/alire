with Ada.Directories;

package body Alire.Origins.Deployers.Child is

   ------------
   -- Deploy --
   ------------

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome is
      pragma Unreferenced (This);
   begin
      Ada.Directories.Create_Path (Folder);
      return Outcome_Success;
   end Deploy;

end Alire.Origins.Deployers.Child;
