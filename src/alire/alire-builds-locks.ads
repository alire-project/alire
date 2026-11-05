private with Ada.Containers.Vectors;
private with Ada.Finalization;

private with AAA.Filesystem.Locks;
private with AAA.Strings;

with Alire.Releases;
limited with Alire.Roots;

package Alire.Builds.Locks is

   --  Locks on shared build folders, so that alr processes building
   --  workspaces with the same dependencies don't break each other's builds.
   --  A folder is locked only while worked on: synced, configured, running
   --  its actions, or compiled, the latter only until it has been built once
   --  (see Builds.Is_Built): afterwards, its sources don't change, so nobody
   --  waits to compile them. Nothing is locked with the setting
   --  dependencies.shared_locking disabled, which is effectively the old
   --  behavior. Errors when simultaneously building a shared dependency
   --  usually manifest as linking errors or outdated ALI files.

   --  A process takes several locks at once only when compiling, and then
   --  in a fixed order and holding no others, so processes can't deadlock.

   type Holder (<>) is tagged limited private;
   --  The locks taken by one of the following, released when it is finalized

   function None return Holder;
   --  Locks nothing

   function Folder (Root    : in out Roots.Root;
                    Release : Releases.Release)
                    return Holder;
   --  Lock the shared build folder of Release only, if it has one

   function For_Compiling (Root    : in out Roots.Root;
                           Release : Releases.Release)
                           return Holder;
   --  Lock the shared build folder of Release, if it has one, and those of
   --  the other releases in the solution not built yet, as compiling Release
   --  may compile them too.

private

   type Lock_Access is access AAA.Filesystem.Locks.Lock;

   package Lock_Vectors is new Ada.Containers.Vectors (Positive, Lock_Access);

   type Holder is new Ada.Finalization.Limited_Controlled with record
      Locks : Lock_Vectors.Vector;
      Names : AAA.Strings.Vector;
   end record;

   overriding procedure Finalize (This : in out Holder);

end Alire.Builds.Locks;
