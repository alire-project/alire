with Alr.OS_Lib; use Alr.OS_Lib;

package body Alr.Git is

   -----------
   -- Clone --
   -----------

   procedure Clone (Source_URL, Target_Folder : String) is
   begin
      Spawn (Command ("git"), "clone", Source_URL, Target_Folder);
   end Clone;

   ----------
   -- Pull --
   ----------

   procedure Pull (Folder : String) is
   begin
      Spawn (Command ("git"), "pull", Folder);
   end Pull;

end Alr.Git;
