with Alire.Containers;
with Alire.Index; use Alire.Index;
with Alire.OS_Lib;
with Alire.Releases;

private with Alr.Hardcoded;

with Semantic_Versioning;

package Alr.Project is

   --  Only file needed from the project alr file (project_alr.ads).
   --  Besides the important Set_Root_Project, unfortunately it renames most of Alire.Index to
   --  make it directly visible in project_alr.ads

   Current : Alire.Containers.Release_H;
   --  Root dependency (the working project). If Is_Empty we know we must recompile,
   --  unless the hash already matches. In this case, we know the project file is
   --  missing the Set_Root_Project call

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

   function Enter_Root (Prj : Alire.Project_Name := Current.Element.Project) return Alire.OS_Lib.Folder_Guard
     with Pre => (not Current.Is_Empty);
   --  Enters the root folder if not already there

   subtype Release is Alire.Releases.Release;

   function Set_Root_Project (Project    : Alire.Project_Name;
                              Version    : Semantic_Versioning.Version;
                              Depends_On : Alire.Index.Dependencies := Alire.Index.No_Dependencies)
                              return Release;
   --  This function must be called in the working project alire file.
   --  Otherwise alr does not know what's the current project, and its version and dependencies
   --  It could be manually parsed from the file, but that's precisely what we want to avoid
   --  The returned Release is the same; this is just a trick to be able to use it in an spec file.

   --  Visibility:

   function V (Semantic_Version : String) return Semantic_Versioning.Version
               renames Semantic_Versioning.New_Version;

   subtype Project_Name is Alire.Project_Name;
   subtype Dependencies is Alire.Index.Dependencies;

   function At_Least_Within_Major (P : Project_Name; V : Version) return Dependencies
                                   renames Alire.Index.At_Least_Within_Major;

   function At_Least  (P : Project_Name; V : Version) return Dependencies renames Alire.Index.At_Least;
   function At_Most   (P : Project_Name; V : Version) return Dependencies renames Alire.Index.At_Most;
   function Less_Than (P : Project_Name; V : Version) return Dependencies renames Alire.Index.Less_Than;
   function More_Than (P : Project_Name; V : Version) return Dependencies renames Alire.Index.More_Than;
   function Exactly   (P : Project_Name; V : Version) return Dependencies renames Alire.Index.Exactly ;
   function Except    (P : Project_Name; V : Version) return Dependencies renames Alire.Index.Except;

   --  Operators, just in case they're used:
   function "and" (VS1, VS2 : Semantic_Versioning.Version_Set) return Semantic_Versioning.Version_Set
                   renames Semantic_Versioning."and";

   use all type Dependencies;

private

   function Name return String is (Current.Constant_Reference.Project);

   function GPR_File (Prj : Alire.Project_Name := Current.Element.Project) return String is
     (Prj & ".gpr");

   function GPR_Alr_File (Prj : Alire.Project_Name := Current.Element.Project) return String is
     (Hardcoded.Build_File (Prj));

end Alr.Project;
