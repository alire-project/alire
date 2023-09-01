with Diskflags;

package Alire.Flags is

   --  Flags are small files we store under the workspace's alire directory to
   --  signal that somethings has succeeded.

   subtype Flag is Diskflags.Flag;

   --  All following `Base` paths point to a release top-dir, not necessarily
   --  to a workspace's root.

   type Alire_Flags is
     (Complete_copy
      --  Signals that a release deployment has completed successfully

      , Post_Fetch_Done
      --  Signals that post-fetch has been run for the release
     );

   function New_Flag (Name : Alire_Flags;
                      Base : Absolute_Path)
                      return Flag;
   --  Base refers to the top-dir of any release, not necessarily a workspace
   --  root.

end Alire.Flags;
