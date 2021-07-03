with Alire.Containers;
with Alire.Releases;

package Alire.Shared is

   --  Stuff about shared/binary crates that are deployed not in the local
   --  workspace but in the shared configuration folder.

   function Available return Containers.Release_Set;
   --  Return the contents of a lockfile describing what releases are installed
   --  at the shared location.

   function Install_Path return Any_Path;
   --  Returns the base folder in which all shared releases live

   procedure Share (Release : Releases.Release);
   --  Deploy a release in the shared location for the configuration

end Alire.Shared;
