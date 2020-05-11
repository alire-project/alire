with Alire.Containers;
with Alire.Interfaces;
with Alire.Properties;
with Alire.TOML_Adapters;

with TOML;

package Alire.Solutions is

   --  A solutions is a set of releases + externals that fulfills the
   --  transitive dependencies of the root crate.

   subtype Dependency_List is Alire.Containers.Dependency_Lists.List;

   subtype Release_Map is Alire.Containers.Release_Map;

   type Solution (Valid : Boolean) is
     new Interfaces.Tomifiable
     and Interfaces.Detomifiable with record
      case Valid is
         when True  =>
            Releases : Release_Map;
            --  Resolved dependencies to be deployed

            Hints    : Dependency_List;
            --  Unresolved external dependencies

         when False =>
            null;
      end case;
   end record;

   Invalid_Solution     : constant Solution;
   Empty_Valid_Solution : constant Solution;

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
     Pre => (for all Release of This.Releases =>
               Release.Dependencies.Is_Unconditional and then
               Release.Properties.Is_Unconditional);
   --  As previous one, but requires releases not to have dynamic expressions

private

   Invalid_Solution     : constant Solution := (Valid => False);
   Empty_Valid_Solution : constant Solution := (Valid => True, others => <>);

end Alire.Solutions;
