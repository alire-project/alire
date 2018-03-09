with Alire.Releases;
with Alire.Root_Release;

with Alr.OS_Lib;

private with Alr.Hardcoded;

package Alr.Release is

   --  Facilities to work with the current project, stored in Alire.Root_Project

   function Current return Alire.Releases.Release renames Alire.Root_Release.Current;

   function Is_Empty return Boolean;

   procedure Check_Valid
     with Post => (not Is_Empty or else raise Command_Failed);
   --  Graceful check that Current contains what it should.

   function Name return String
     with Pre => (not Is_Empty);

   function Build_File (Prj : Alire.Project_Name := Current.Project) return String
     with Pre => (not Is_Empty);
   --  The alr environment project file (project_alr.gpr)

   function Enter_Root (Prj : Alire.Project_Name := Current.Project) return OS_Lib.Folder_Guard
     with Pre => (not Is_Empty);
   --  Enters the root folder if not already there

private

   function Is_Empty return Boolean is (not Alire.Root_Release.Is_Set);

   function Name return String is (Current.Project);

   function Build_File (Prj : Alire.Project_Name := Current.Project) return String is
     (Hardcoded.Build_File (Prj));

end Alr.Release;
