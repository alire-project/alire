package Alire.Roots.Locks is

   --  Only one alr process at a time works on a workspace, as they would
   --  otherwise break each other's files. Others wait for their turn. The OS
   --  releases the lock when the holder ends, however that happens.

   procedure Acquire (This : Root);
   --  Lock the workspace of This for the rest of the run, waiting while
   --  another alr process holds it. Does nothing if already held, or if the
   --  holder is the alr process that started this one (e.g. through an
   --  action), which would otherwise wait for its child forever.

   procedure Release;
   --  Before handing over to a program that doesn't need the workspace kept
   --  locked, e.g. `alr run` and `alr exec`, so that alr can be used in the
   --  meantime. Noop if not held.

end Alire.Roots.Locks;
