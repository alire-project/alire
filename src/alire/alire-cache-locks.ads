with AAA.Filesystem.Locks;

package Alire.Cache.Locks is

   --  Locks on things in the cache shared by workspaces, so that alr
   --  processes working on different workspaces at the same time don't
   --  step on each other shared builds.

   procedure Acquire (Lock  : in out AAA.Filesystem.Locks.Lock;
                      Name  : Relative_Path;
                      Doing : String)
     with Pre => not Lock.Is_Held;
   --  Lock exclusively the file Name inside the locks folder, for as long as
   --  Lock holds it. If another alr process holds it, say so, with Doing
   --  describing what it is doing (e.g. "deploying hello=1.0.0"), and wait. If
   --  the lock file can't be created, e.g. in a read-only cache, go on
   --  without locking.

end Alire.Cache.Locks;
