with Alire.Errors;
with Alire.Milestones;
with Alire.Releases.Containers;

with CLIC.User_Input;

package Alire.Shared is

   --  Stuff about shared/binary crates that are deployed not in the local
   --  workspace but in the shared configuration folder.

   function Available return Releases.Containers.Release_Set;
   --  Returns the releases installed at the shared location

   function Release (Target : Milestones.Milestone) return Releases.Release;
   --  Retrieve the release corresponding to Target, if it exists. Will raise
   --  Constraint_Error if not among Available.

   function Install_Path return Any_Path;
   --  Returns the base folder in which all shared releases live

   procedure Share (Release  : Releases.Release;
                    Location : Any_Path := Install_Path);
   --  Deploy a release in the specified location

   procedure Remove
     (Release : Releases.Release;
      Confirm : Boolean := not CLIC.User_Input.Not_Interactive)
     with Pre => Available.Contains (Release)
     or else raise Checked_Error with
       Errors.Set ("Requested release is not installed: "
                   & Release.Milestone.TTY_Image);
   --  Remove a release from the shared location for the configuration

   procedure Remove
     (Target : Milestones.Milestone;
      Confirm : Boolean := not CLIC.User_Input.Not_Interactive);
   --  Behaves as the previous Remove

end Alire.Shared;
