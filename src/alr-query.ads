with Alire.Containers;
with Alire.Index;
with Alire.Types;

with Semantic_Versioning;

package Alr.Query is

   type Policies is (Oldest, Newest);

   use Alire;

   --subtype Solution is Containers.Version_Map; -- A dependence-valid mapping of project -> version
   subtype Instance is Containers.Release_Map; -- A list of releases complying with a Solution
   subtype Release  is Index.Release;

   Empty_Instance : constant Instance := Containers.Project_Release_Maps.Empty_Map;

   function Exists (Project : Project_Name;
                    Version : Semantic_Versioning.Version)
                    return Boolean renames Alire.Index.Exists;

   function Find (Project : Project_Name;
                  Version : Semantic_Versioning.Version) return Release renames Alire.Index.Find;

   function Exists (Project : Project_Name;
                    Allowed : Semantic_Versioning.Version_Set := Semantic_Versioning.Any)
                    return Boolean;

   function Find (Project : Project_Name;
                  Allowed : Semantic_Versioning.Version_Set := Semantic_Versioning.Any;
                  Policy  : Policies) return Release;

   function Resolve (Deps    :     Alire.Types.Platform_Dependencies;
                     Success : out Boolean;
                     Policy  :     Policies) return Instance;

   procedure Print_Solution (I : Instance);


   function Dependency_Image (Project  : Project_Name;
                              Versions : Semantic_Versioning.Version_Set;
                              Policy   : Policies := Newest) return String;

end Alr.Query;
