with Alire.Index;
with Alire.Properties;
with Alire.Solutions;
with Alire.TOML_Adapters;
with Alire.Types;

with Semantic_Versioning.Extended;

with TOML;

package Alire.Solver is

   --------------
   -- Policies --
   --------------

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

   subtype Release  is Types.Release;

   subtype Solution is Solutions.Solution;

   --  The dependency solver receives a list of dependencies and will return
   --  either a valid solution if one can be found (exploration is exhaustive).
   --  System dependencies are resolved in platforms with system packager
   --  support. Otherwise they're filed as "hints" but do not cause a failure
   --  in resolution. In this case, a warning will be provided for the user
   --  with a list of the dependencies that are externally required.

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
                     Props   : Properties.Vector;
                     Current : Solution;
                     Options : Query_Options := Default_Options)
                     return Solution;
   --  Exhaustively look for a solution to the given dependencies, under the
   --  given platform properties and lookup options. A current solution may
   --  be given and pinned releases will be reused.

   function Is_Resolvable (Deps    : Types.Platform_Dependencies;
                           Props   : Properties.Vector;
                           Current : Solution;
                           Options : Query_Options := Default_Options)
                           return Boolean;
   --  Simplified call to Resolve, discarding result

   -------------------
   -- Debug helpers --
   -------------------

   procedure Print_Solution (Sol : Solution);

   function Dependency_Image
     (Name     : Alire.Crate_Name;
      Versions : Semantic_Versioning.Extended.Version_Set;
      Policy   : Age_Policies := Newest) return String;

end Alire.Solver;
