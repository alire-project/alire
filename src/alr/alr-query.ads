with Alire.Containers;
with Alire.Index;
with Alire.Properties;
with Alire.Types;

with Semantic_Versioning;

package Alr.Query is

   type Policies is (Oldest, Newest);

   use Alire;

   --  subtype Solution is Containers.Version_Map;
   --  A dependence-valid mapping of project -> version

   subtype Instance is Containers.Release_Map;
   --  A list of releases complying with a Solution

   subtype Release  is Types.Release;

   type Solution (Valid : Boolean) is record
      case Valid is
         when True  => Releases : Instance;
         when False => null;
      end case;
   end record;

   Empty_Instance : constant Instance :=
     (Containers.Project_Release_Maps.Empty_Map with null record);

   ---------------------
   --  Basic queries  --
   --  Merely check the catalog

   function Exists (Project : Alire.Project;
                    Version : Semantic_Versioning.Version)
                    return Boolean renames Alire.Index.Exists;

   function Find (Project : Alire.Project;
                  Version : Semantic_Versioning.Version)
                  return Release
   renames Alire.Index.Find;

   function Exists
     (Project : Alire.Project;
      Allowed : Semantic_Versioning.Version_Set := Semantic_Versioning.Any)
      return Boolean;

   function Find
     (Project : Alire.Project;
      Allowed : Semantic_Versioning.Version_Set := Semantic_Versioning.Any;
      Policy  : Policies)
      return Release;

   function Find (Project : String;
                  Policy  : Policies) return Release;
   --  Given a textual project+set (see Parsers), find the release if it exists

   ----------------------------------
   --  Platform individual queries --
   --  Only need a release and the platform properties

   function Is_Available (R : Alire.Index.Release) return Boolean;
   --  The release knows the requisites on the platform; here we evaluate these
   --  against the current platform. Current checks include the "available"
   --  requisites and that the native package do exist. NOTE: it does not
   --  consider that dependencies can be resolved, only that it "could" be
   --  available.

   -----------------------
   --  Advanced queries --
   --  They may need to travel the full catalog, with multiple individual
   --  availability checks.

   function Resolve (Deps    :     Alire.Types.Platform_Dependencies;
                     Policy  :     Policies) return Solution;

   function Is_Resolvable (Deps : Types.Platform_Dependencies) return Boolean;
   --  simplified call to Resolve, discarding result

   procedure Print_Solution (I : Instance);

   function Dependency_Image (Project  : Alire.Project;
                              Versions : Semantic_Versioning.Version_Set;
                              Policy   : Policies := Newest) return String;

end Alr.Query;
