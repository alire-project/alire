with Ada.Containers.Indefinite_Ordered_Maps;

with Alire.Conditional;
with Alire.Interfaces;
with Alire.Properties;
with Alire.Requisites;
with Alire.TOML_Adapters;

package Alire.Projects with Preelaborate is

   --  TODO: Rename to Crates (Issue #113)

   package Project_Description_Maps
   is new Ada.Containers.Indefinite_Ordered_Maps
     (Alire.Project, Description_String);

   --  TODO: combine Index, Descriptions in a single data structure
   Descriptions : Project_Description_Maps.Map;
   --  Master list of known projects & descriptions

   type Named is limited interface;

   function Project (N : Named) return Alire.Project is abstract;

   type Sections is (General_Section,  -- In [general]
                     Release_Section); -- In a release

   -------------
   -- General --
   -------------

   --  A crate contains mandatory information, some of it overridable by its
   --  releases. See doc/catalog-format-spec.rst.

   type General is new Interfaces.Detomifiable with private;
   --  The General portion of a Crate

   overriding
   function From_TOML (This : in out General;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome;

   --  TODO: Releases.Release should probable inherit from General.

private

   type General is new Interfaces.Detomifiable with record
      Properties   : Conditional.Properties;
      Dependencies : Conditional.Dependencies;
      Forbidden    : Conditional.Dependencies;

      Available    : Requisites.Tree;
   end record;

end Alire.Projects;
