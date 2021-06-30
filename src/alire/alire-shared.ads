with Alire.Releases;
with Alire.Solutions;

package Alire.Shared is

   --  Stuff about shared/binary crates that are deployed not in the local
   --  workspace but in the shared configuration folder.

   function Available return Solutions.Solution;
   --  Return the contents of a lockfile describing what releases are installed
   --  at the shared location.

   procedure Share (Release : Releases.Release);
   --  Deploy a release in the shared location for the configuration

end Alire.Shared;
