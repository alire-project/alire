with Alire.Containers;
with Alire.Index;
with Alire.Properties;
with Alire.Types;

with Semantic_Versioning;

package Alr.Query is

   type Age_Policies is (Oldest, Newest);
   --  When looking for releases within a crate, which one to try first.

   type Native_Policies is (Hint, Fail);
   --  * Hint: attempt to normally resolve native crates. When impossible,
   --  store such crate as a hint and assume it is available.
   --  * Fail: treat native crates normally and fail if unavailable.

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
      Policy  : Age_Policies)
      return Release;

   function Find (Project : String;
                  Policy  : Age_Policies) return Release;
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

   type Query_Options is record
      Age    : Age_Policies    := Newest;
      Native : Native_Policies := Hint;
   end record;

   Default_Options : constant Query_Options := (others => <>);

   function Resolve (Deps    : Alire.Types.Platform_Dependencies;
                     Options : Query_Options := Default_Options)
                     return Solution;

   function Is_Resolvable (Deps : Types.Platform_Dependencies) return Boolean;
   --  simplified call to Resolve, discarding result

   -------------------
   -- Debug helpers --
   -------------------

   procedure Print_Solution (Sol : Solution);

   function Dependency_Image (Project  : Alire.Project;
                              Versions : Semantic_Versioning.Version_Set;
                              Policy   : Age_Policies := Newest) return String;

end Alr.Query;
