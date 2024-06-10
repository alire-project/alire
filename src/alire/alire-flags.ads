with Diskflags;

package Alire.Flags is

   --  Flags are empty files we store under the workspace's alire directory to
   --  signal that some operation has succeeded.

   subtype Flag is Diskflags.Flag;

   --  All following `Base` paths point to a release top-dir, not necessarily
   --  to a workspace's root.

   function Complete_Copy (Base : Absolute_Path) return Flag;
   --  Signals that a release deployment has completed successfully

   function Post_Fetch (Base : Absolute_Path) return Flag;
   --  Signals that post-fetch has been run for the release

private

   --  The following names directly translate into lowercase filenames
   type Names is
     (Complete_Copy,
      Post_Fetch_Done);

   function New_Flag (Name : Names;
                      Base : Absolute_Path)
                      return Flag;
   --  Base refers to the top-dir of any release, not necessarily a workspace
   --  root.

   function Complete_Copy (Base : Absolute_Path) return Flag
   is (New_Flag (Complete_Copy, Base));

   function Post_Fetch (Base : Absolute_Path) return Flag
   is (New_Flag (Post_Fetch_Done, Base));

end Alire.Flags;
