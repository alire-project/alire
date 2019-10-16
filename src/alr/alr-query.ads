with Alire.Containers;
with Alire.Index;
with Alire.Properties;
with Alire.Types;

with Semantic_Versioning;

package Alr.Query is

   type Policies is (Oldest, Newest);

   use Alire;

   subtype Dep_List is Alire.Containers.Dependency_Lists.List;
   --  Dependency lists are used to keep track of failed dependencies

   subtype Instance is Alire.Containers.Release_Map;
   --  A list of releases complying with a Solution

   subtype Release  is Types.Release;

   --  The dependency solver receives a list of dependencies and will return
   --  either a valid solution if one can be found (exploration is exhaustive).
   --  Native dependencies are resolved in platforms with native packager
   --  support. Otherwise they're filed as "hints" but do not cause a failure
   --  in resolution. In this case, a warning will be provided for the user
   --  with a list of the dependencies that are externally required.

   type Solution (Valid : Boolean) is record
      case Valid is
         when True  =>
            Releases : Instance; -- Resolved dependencies to be deployed
            Hints    : Dep_List; -- Unresolved native dependencies

         when False =>
            null;
      end case;
   end record;

   Empty_Deps : constant Dep_List :=
                  Alire.Containers.Dependency_Lists.Empty_List;

   Empty_Instance : constant Instance :=
     (Alire.Containers.Project_Release_Maps.Empty_Map with null record);

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
