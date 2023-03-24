with Alire.Errors;
with Alire.Milestones;
with Alire.Releases.Containers;

with CLIC.User_Input;

package Alire.Shared is

   --  Stuff about shared/binary crates that are deployed not in the local
   --  workspace but in the shared configuration folder.

   function Available (Detect_Externals : Boolean := True)
                       return Releases.Containers.Release_Set;
   --  Returns the releases installed at the shared location

   function Release (Target : Milestones.Milestone;
                     Detect_Externals : Boolean := True)
                     return Releases.Release;
   --  Retrieve the release corresponding to Target, if it exists. Will raise
   --  Constraint_Error if not among Available.

   function Path return Any_Path;
   --  Returns the base folder in which all shared releases live:
   --  * <config>/cache/dependencies if set with --config/-c
   --  * <config>/cache/dependencies if set through ALR_CONFIG
   --  * ~/.cache/alire/dependencies by default

   procedure Set_Path (Path : Absolute_Path);
   --  Override the location of the global cache location

   procedure Share (Release  : Releases.Release;
                    Location : Any_Path := Path);
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
