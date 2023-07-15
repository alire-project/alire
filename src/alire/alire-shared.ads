with Alire.Errors;
with Alire.Milestones;
with Alire.Releases.Containers;

with CLIC.User_Input;

package Alire.Shared is

   --  Since the new shared builds infrastructure, this applies exclusively
   --  to releases that belong to a toolchain. TODO: migrate it to either
   --  Alire.Toolchains or Alire.Toolchains.Shared.

   function Available (Detect_Externals : Boolean := True)
                       return Releases.Containers.Release_Set;
   --  Returns the releases installed at the shared location

   function Release (Target : Milestones.Milestone;
                     Detect_Externals : Boolean := True)
                     return Releases.Release;
   --  Retrieve the release corresponding to Target, if it exists. Will raise
   --  Constraint_Error if not among Available.

   function Path return Any_Path;
   --  Returns the base folder in which all shared releases live, defaults to
   --  <cache>/toolchains

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
