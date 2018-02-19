with Ada.Directories;

with Alire.OS_Lib;

package body Alr.Origins is

   type Fetcher is access procedure (From : Alire.Origins.Origin; Folder : String);

   ---------
   -- Apt --
   ---------

   procedure Apt (From : Alire.Origins.Origin; Folder : String) is
      pragma Unreferenced (Folder);
   begin
      Trace.Always ("sudo needed to install platform package " & From.Id);
      Alire.OS_Lib.Spawn_Bypass ("sudo", "apt-get install -q -q -y " & From.Id);
   exception
      when others =>
         Trace.Error ("Installation of native package " & From.Id & " failed");
         raise;
   end Apt;

   ----------
   -- Fail --
   ----------

   procedure Fail (From : Alire.Origins.Origin; Folder : String) is
   begin
      raise Program_Error with "Should never be requested";
   end Fail;

   ---------
   -- Git --
   ---------

   procedure Git (From : Alire.Origins.Origin; Folder : String) is
   begin
      Trace.Info ("Checking out: " & From.URL);
      Alire.OS_Lib.Spawn ("git", "clone -n -q --progress " & From.URL & " " & Folder);

      declare
         use Ada.Directories;
         Parent : constant String := Current_Directory;
      begin
         Set_Directory (Folder);
         Alire.OS_Lib.Spawn ("git", "reset --hard -q " & From.Id);
         Set_Directory (Parent);
      end;
   end Git;

   use all type Alire.Origins.Kinds;

   Fetchers : constant array (Alire.Origins.Kinds) of Fetcher :=
                (Filesystem => Fail'Access,
                 Git        => Git'Access,
                 Local_Apt  => Apt'Access);

   -----------
   -- Fetch --
   -----------

   procedure Fetch (From : Alire.Origins.Origin; Folder : String) is
   begin
      Fetchers (From.Kind).all (From, Folder);
   exception
      when others =>
         Trace.Error ("Deployment of " & From.Id & " from " & From.URL & " to " & Folder & " failed");
         if Ada.Directories.Exists (Folder) then
            Ada.Directories.Delete_Tree (Folder);
         end if;
         raise;
   end Fetch;


end Alr.Origins;
