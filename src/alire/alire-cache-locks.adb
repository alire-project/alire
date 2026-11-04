with Ada.Exceptions;
with Ada.IO_Exceptions;

with Alire.Directories;
with Alire.Paths;

package body Alire.Cache.Locks is

   use Directories.Operators;

   -------------
   -- Acquire --
   -------------

   procedure Acquire (Lock  : in out AAA.Filesystem.Locks.Lock;
                      Name  : Relative_Path;
                      Doing : String)
   is
      File : constant Absolute_Path :=
               Cache.Path / Paths.Locks_Folder_Inside_Cache_Folder / Name;
   begin
      Directories.Create_Tree (Directories.Parent (File));

      if not Lock.Try_Acquire (File) then
         Trace.Warning ("Waiting for another alr process " & Doing & "...");
         Lock.Acquire (File);
      end if;

      Trace.Debug ("Locked: " & File);
   exception
      when E : Ada.IO_Exceptions.Use_Error =>
         Trace.Debug ("Going on unlocked: "
                      & Ada.Exceptions.Exception_Message (E));
   end Acquire;

end Alire.Cache.Locks;
