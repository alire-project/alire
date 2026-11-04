with AAA.Filesystem.Locks;

with Alire.Directories;
with Alire.Environment;
with Alire.OS_Lib;
with Alire.Paths;

package body Alire.Roots.Locks is

   use Directories.Operators;

   File_Name : String renames Paths.Workspace_Lock_Inside_Working_Folder;

   Holder_Var : String renames Alire.Environment.Workspace_Lock;
   --  Holds the workspace path while this process holds its lock, so alr
   --  processes started from this one know it

   Lock : AAA.Filesystem.Locks.Lock;

   -------------
   -- Acquire --
   -------------

   procedure Acquire (This : Root) is
      Path : constant Absolute_Path := Roots.Path (This);
      File : constant Absolute_Path := This.Working_Folder / File_Name;
   begin
      if Lock.Is_Held then
         return;
      end if;

      if OS_Lib.Getenv (Holder_Var) = Path then
         Trace.Debug ("Workspace already locked by parent alr: " & Path);
         return;
      end if;

      Directories.Create_Tree (This.Working_Folder);

      if not Lock.Try_Acquire (File) then
         Trace.Warning
           ("Waiting for another alr process using this workspace...");
         Lock.Acquire (File);
      end if;

      OS_Lib.Setenv (Holder_Var, Path);
      Trace.Debug ("Workspace locked: " & File);
   end Acquire;

   -------------
   -- Release --
   -------------

   procedure Release is
   begin
      if Lock.Is_Held then
         Lock.Release;
         OS_Lib.Setenv (Holder_Var, "");
         Trace.Debug ("Workspace unlocked");
      end if;
   end Release;

end Alire.Roots.Locks;
