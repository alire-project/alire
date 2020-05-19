with Alire.Conditional;
with Alire.Containers;
with Alire.Interfaces;
with Alire.Properties;
with Alire.Releases;
with Alire.TOML_Adapters;

limited with Alire.Solutions.Diffs;

with TOML;

package Alire.Solutions is

   --  A solutions is a set of releases + externals that fulfills the
   --  transitive dependencies of the root crate.

   subtype Dependency_Map is Alire.Containers.Dependency_Map;

   subtype Release_Map is Alire.Containers.Release_Map;

   type Solution (Valid : Boolean) is
     new Interfaces.Tomifiable
     and Interfaces.Detomifiable with private;

   Invalid_Solution     : constant Solution;
   Empty_Valid_Solution : constant Solution;

   function New_Solution (Releases : Release_Map;
                          Hints    : Dependency_Map)
                          return Solution;
   --  A new valid solution

   function Releases (This : Solution) return Release_Map with
     Pre => This.Valid;
   --  Returns the regular releases that conform a solution

   function Hints (This : Solution) return Dependency_Map with
     Pre => This.Valid;
   --  Returns dependencies that will have to be fulfilled externally. These
   --  correspond to undetected externals; a detected external results in a
   --  regular release and should require no user action.

   function Changes (Former, Latter : Solution) return Diffs.Diff;

   function Required (This : Solution) return Containers.Crate_Name_Sets.Set;
   --  Retrieve all required crates in the solution, no matter if they have
   --  known releases or only hints. Will return an empty set for invalid
   --  solutions. TODO: when we track reasons for solving failure, return
   --  the required crates with their reason for non-solvability.

   function Changing_Pin (This   : Solution;
                          Name   : Crate_Name;
                          Pinned : Boolean) return Solution;
   --  Return a copy of the solution with the new pinning status of Name

   function Pins (This : Solution) return Conditional.Dependencies;
   --  Return all pinned releases as exact version dependencies. Will return an
   --  empty list for invalid solutions.

   function Pins (This : Solution) return Release_Map;
   --  Return all pinned release. Will return an empty map for invalid
   --  solutions.

   function With_Pins (This, Src : Solution) return Solution;
   --  Copy pins from Src to This

   procedure Print (This     : Solution;
                    Root     : Alire.Releases.Release;
                    Env      : Properties.Vector;
                    Detailed : Boolean;
                    Level    : Trace.Levels);
   --  Prints releases, and direct and transitive dependencies. Root is the
   --  crate not in solution that introduces the direct dependencies. When
   --  Detailed, extra information about origins is shown.

   procedure Print_Pins (This : Solution);
   --  Dump a table with pins in this solution

   --  TOML-related subprograms

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Solution;
   --  Since Solution is unconstrained this allows loading of both
   --  valid/invalid solutions.

   overriding
   function From_TOML (This : in out Solution;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome
     with Pre  => This.Valid,
          Post => From_TOML'Result.Success;
   --  As this function is used to load Alire-generated files, the only
   --  possible outcome when properly used is Success. Any unexpected
   --  situation will result in uncaught exception.

   function To_TOML (This  : Solution;
                     Props : Properties.Vector) return TOML.TOML_Value;
   --  Stores a solution as a TOML file. Since dynamic expression export is
   --  unimplemented yet, we use the given properties to localize to current
   --  platform. TODO: export cases (this is the same limitation that exists
   --  for the regular export of crate.toml)

   overriding
   function To_TOML (This : Solution) return TOML.TOML_Value with
     Pre => not This.Valid or else
           (for all Release of This.Releases =>
               Release.Dependencies.Is_Unconditional and then
               Release.Properties.Is_Unconditional);
   --  As previous one, but requires releases not to have dynamic expressions

private

   type Solution (Valid : Boolean) is
     new Interfaces.Tomifiable
     and Interfaces.Detomifiable with record
      case Valid is
         when True  =>
            Releases : Release_Map;
            --  Resolved dependencies to be deployed

            Hints    : Dependency_Map;
            --  Unresolved external dependencies
         when False =>
            null;
      end case;
   end record;

   Invalid_Solution     : constant Solution := (Valid => False);
   Empty_Valid_Solution : constant Solution := (Valid => True, others => <>);

   function New_Solution (Releases : Release_Map;
                          Hints    : Dependency_Map)
                          return Solution
   is (Solution'(Valid    => True,
                 Releases => Releases,
                 Hints    => Hints));

   function Hints (This : Solution) return Dependency_Map
   is (This.Hints);

   function Releases (This : Solution) return Release_Map
   is (This.Releases);

end Alire.Solutions;
