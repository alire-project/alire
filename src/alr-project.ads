with Alire.Containers;
with Alire.Releases;
with Alire.Root_Project;

with Alr.OS_Lib;

private with Alr.Hardcoded;

package Alr.Project is

   --  Facilities to work with the current project, stored in Alire.Root_Project

   Current : Alire.Containers.Release_H renames Alire.Root_Project.Current;

   procedure Check_Valid
     with Post => (not Current.Is_Empty or else raise Command_Failed);
   --  Graceful check that Current contains what it should.

   function Name return String
     with Pre => (not Current.Is_Empty);

   function GPR_File (Prj : Alire.Project_Name := Current.Element.Project) return String
     with Pre => (not Current.Is_Empty);
   --  The actual project root file (not the _alrbuild one!)

   function GPR_Alr_File (Prj : Alire.Project_Name := Current.Element.Project) return String
     with Pre => (not Current.Is_Empty);
   --  The alr environment project file (project_alr.gpr)

   function Enter_Root (Prj : Alire.Project_Name := Current.Element.Project) return OS_Lib.Folder_Guard
     with Pre => (not Current.Is_Empty);
   --  Enters the root folder if not already there

private

   function Name return String is (Current.Constant_Reference.Project);

   function GPR_File (Prj : Alire.Project_Name := Current.Element.Project) return String is
     (Prj & ".gpr");

   function GPR_Alr_File (Prj : Alire.Project_Name := Current.Element.Project) return String is
     (Hardcoded.Build_File (Prj));

end Alr.Project;
