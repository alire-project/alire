with Alire.Containers;
with Alire.Index;
with Alire.Properties;
with Alire.Properties.Dependencies;
with Alire.Types;

private with Alr.Origins;
private with Alr.OS;

with Semantic_Versioning;

package Alr.Query is

   type Policies is (Oldest, Newest);

   Platform_Properties : constant Alire.Properties.Vector;
   --  OS properties plus native resolvability checker

   use Alire;

   --subtype Solution is Containers.Version_Map; -- A dependence-valid mapping of project -> version
   subtype Instance is Containers.Release_Map; -- A list of releases complying with a Solution
   subtype Release  is Index.Release;

   Empty_Instance : constant Instance := Containers.Project_Release_Maps.Empty_Map;

   ---------------------
   --  Basic queries  --
   --  Merely check the catalog

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

   ----------------------------------
   --  Platform individual queries --
   --  Only need a release and the platform properties

   function Is_Available (R : Alire.Index.Release) return Boolean;
   --  The release knows the requisites on the platform; here we evaluate these against the current platform
   --  Current checks include the "available" requisites and that the native package do exist

   -----------------------
   --  Advanced queries --
   --  They may need to travel the full catalog, with multiple individual availability checks

   function Resolve (Deps    :     Alire.Types.Platform_Dependencies;
                     Success : out Boolean;
                     Policy  :     Policies) return Instance;

   function Is_Resolvable (Deps : Types.Platform_Dependencies) return Boolean;
   --  simplified call to Resolve, discarding result

   procedure Print_Solution (I : Instance);


   function Dependency_Image (Project  : Project_Name;
                              Versions : Semantic_Versioning.Version_Set;
                              Policy   : Policies := Newest) return String;

private

   use all type Alire.Properties.Vector;

   Platform_Properties : constant Alire.Properties.Vector :=
                  OS.Properties and
                  Alire.Properties.Dependencies.New_Property (Query.Is_Resolvable'Access,
                                                              OS.Properties);

   function Is_Available (R : Alire.Index.Release) return Boolean is
     (R.Available.Check (Platform_Properties) and then
          (if R.Origin.Is_Native
           then Origins.Native_Package_Exists (R.Origin.Package_Name (OS.Distribution))));
   --  R.Available requisites are checked against platform properties
   --  Then, if the origin is native, which implies conditional availability too, this is checked
   --  NOTE: this does not check that dependencies can be resolved!

end Alr.Query;
