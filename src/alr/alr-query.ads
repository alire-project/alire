with Alire.Containers;
with Alire.Index;
with Alire.Properties;
with Alire.Types;

with Semantic_Versioning.Extended;

package Alr.Query is

   type Age_Policies is (Oldest, Newest);
   --  When looking for releases within a crate, which one to try first.

   type Detection_Policies is (Detect, Dont_Detect);
   --  * Detect: externals will be detected and added to the index once needed.
   --  * Dont_Detect: externals will remain undetected (faster).

   type Hinting_Policies is (Hint, Fail);
   --  * Hint: any crate with externals, detected or not, will as last resort
   --  provide a hint.
   --  * Fail: fail for any unsatisfiable crate. If Detect, externally detected
   --  releases will be used normally; otherwise a crate with only externals
   --  will always cause failure.

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

   type Solution (Valid : Boolean) is tagged record
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
     (Alire.Containers.Crate_Release_Maps.Empty_Map with null record);

   ---------------------
   --  Basic queries  --
   --  Merely check the catalog

   function Exists (Name    : Alire.Crate_Name;
                    Version : Semantic_Versioning.Version)
                    return Boolean renames Alire.Index.Exists;

   function Find (Name    : Alire.Crate_Name;
                  Version : Semantic_Versioning.Version)
                  return Release
   renames Alire.Index.Find;

   function Exists
     (Name    : Alire.Crate_Name;
      Allowed : Semantic_Versioning.Extended.Version_Set :=
        Semantic_Versioning.Extended.Any)
      return Boolean;

   function Find
     (Name    : Alire.Crate_Name;
      Allowed : Semantic_Versioning.Extended.Version_Set :=
        Semantic_Versioning.Extended.Any;
      Policy  : Age_Policies)
      return Release;

   function Find (Name    : String;
                  Policy  : Age_Policies) return Release;
   --  Given a textual crate+set (see Parsers), find the release if it exists

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
      Age       : Age_Policies       := Newest;
      Detecting : Detection_Policies := Detect;
      Hinting   : Hinting_Policies   := Hint;
   end record;

   Default_Options : constant Query_Options := (others => <>);

   function Resolve (Deps    : Alire.Types.Platform_Dependencies;
                     Options : Query_Options := Default_Options)
                     return Solution;

   function Is_Resolvable (Deps    : Types.Platform_Dependencies;
                           Options : Query_Options := Default_Options)
                           return Boolean;
   --  simplified call to Resolve, discarding result

   -------------------
   -- Debug helpers --
   -------------------

   procedure Print_Solution (Sol : Solution);

   function Dependency_Image
     (Name     : Alire.Crate_Name;
      Versions : Semantic_Versioning.Extended.Version_Set;
      Policy   : Age_Policies := Newest) return String;

end Alr.Query;
