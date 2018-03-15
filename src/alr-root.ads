with Alire.Projects;
with Alire.Root;
with Alire.Roots;

with Alr.Hardcoded;
with Alr.OS_Lib;

package Alr.Root is

   function Current return Alire.Roots.Root renames Alire.Root.Current;

   function Is_Empty return Boolean;

   procedure Check_Valid
     with Post => (not Is_Empty or else raise Command_Failed);
   --  Graceful check that Current contains what it should.

   function Is_Released return Boolean;

   function Name return Alire.Projects.Names
     with Pre => (not Is_Empty and then Current.Is_Released);

   function Image return Alire.Name_String
     with Pre => (not Is_Empty);

   function Build_File (Prj : Alire.Name_String := Image) return String
     with Pre => (not Is_Empty);
   --  The alr environment project file (project_alr.gpr)

   function Enter_Root (Prj : Alire.Name_String := Image) return OS_Lib.Folder_Guard
     with Pre => (not Is_Empty);
   --  Enters the root folder if not already there

private

   function Is_Empty return Boolean is (not Alire.Root.Is_Set);

   function Is_Released return Boolean is (Current.Is_Released);

   function Name return Alire.Projects.Names is (Current.Release.Name);

   function Image return Alire.Name_String is (Current.Name);

   function Build_File (Prj : Alire.Name_String := Image) return String is
     (Hardcoded.Build_File (Prj));

end Alr.Root;
