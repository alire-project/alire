with Ada.Directories;

with Alr.OS_Lib;
with Alr.Spawn;

package body Alr.Origins is

   type Fetcher is access procedure (From : Alire.Origins.Origin; Folder : String);

   ---------
   -- Apt --
   ---------

   procedure Apt (From : Alire.Origins.Origin; Folder : String) is
      pragma Unreferenced (Folder);
   begin
      Trace.Always ("sudo needed to install platform package " & From.Id);
      OS_Lib.Spawn_Raw ("sudo", "apt-get install -q -q -y " & From.Id);
   exception
      when others =>
         Trace.Error ("Installation of native package " & From.Id & " failed");
         raise Command_Failed;
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
      Spawn.Command ("git", "clone -n -q --progress " & From.URL & " " & Folder);
      declare
         Guard : constant OS_Lib.Folder_Guard := Os_Lib.Enter_Folder (Folder) with Unreferenced;
      begin
         Spawn.Command ("git", "reset --hard -q " & From.Id);
      end;
   exception
      when others =>
         raise Command_Failed;
   end Git;

   --------
   -- Hg --
   --------

   procedure Hg (From : Alire.Origins.Origin; Folder : String) is
   begin
      Trace.Info ("Checking out: " & From.URL);
      Spawn.Command ("hg", "clone -v -y -u " & From.Id & " " & From.URL & " " & Folder);
   exception
      when others =>
         raise Command_Failed;
   end Hg;

   use all type Alire.Origins.Kinds;

   Fetchers : constant array (Alire.Origins.Kinds) of Fetcher :=
                (Filesystem => Fail'Access,
                 Git        => Git'Access,
                 Hg         => Hg'Access,
                 Apt        => Apt'Access);

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
