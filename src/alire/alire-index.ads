private with Alire_Early_Elaboration;
pragma Unreferenced (Alire_Early_Elaboration);

with Alire.Conditional;
with Alire.Containers;
with Alire.Dependencies;
with Alire.GPR;
with Alire.Origins;
with Alire.Properties;
with Alire.Properties.Licenses;
with Alire.Releases;
with Alire.Requisites;
with Alire.Utils;

with Semantic_Versioning;

package Alire.Index is

   Community_Repo : constant URL :=
                      "git+https://github.com/alire-project/alire-index";
   --  Default index installed on first run

   Community_Name : constant Restricted_Name := "community";

   --  We have two sets of branches in the alire-index repo:
   --  devel-x.x.x and stable-x.x.x

   --  Stable alr use a stable-x.x.x (might be shared across alr versions, if
   --  no changes to the index format are necessary). The development versions
   --  of alr may branch out from the latest stable- to a devel- branch for
   --  breaking changes.

   subtype Branch_String is String with Dynamic_Predicate =>
     Branch_String (Branch_String'First) /= '-'
     and then Branch_String (Branch_String'Last) /= '-'
     and then (for some C of Branch_String => C = '-');

   Community_Branch : constant String := "devel-0.1";
   --  The branch used for the community index

   Version : constant Semantic_Versioning.Version :=
               Semantic_Versioning.New_Version
                 (Utils.Tail (Community_Branch, '-'));
   --  The index version understood by alire must match the one in the indexes
   --  being loaded.

   ---------------
   --  CATALOG  --
   ---------------

   Catalog : Containers.Release_Set;

   type Catalog_Entry (<>) is tagged private;
   --  Used to force the declaration of a single variable to refer to a
   --  project in index specs.
   --  TODO: when the Catalog global is removed, obsolesce this type that
   --  currently only serves to add an entry to said set.

   function Manually_Catalogued_Project
     (Crate_Name, Description : String) return Catalog_Entry;
   --  Helper to programmatically create entries

   -----------------
   -- Index types --
   -----------------

   subtype Release_Dependencies is Conditional.Dependencies;
   subtype Release_Properties   is Conditional.Properties;
   subtype Release_Requisites   is Requisites.Tree;

   No_Dependencies : constant Release_Dependencies :=
     Conditional.For_Dependencies.Empty;
   No_Properties   : constant Release_Properties   :=
     Conditional.For_Properties.Empty;
   No_Requisites   : constant Requisites.Tree      :=
     Requisites.Trees.Empty_Tree;
   No_Origin       : constant Origins.Origin       :=
     Origins.New_Filesystem ("/unavailable");
   No_Version      : constant Semantic_Versioning.Version :=
     Semantic_Versioning.Relaxed ("0");

   subtype Release is Alire.Releases.Release;

   function Register
     ( --  Mandatory
       This               : Catalog_Entry;
       Version            : Semantic_Versioning.Version;
       Origin             : Origins.Origin;
       -- we force naming beyond this point with this ugly guard:
       XXXXXXXXXXXXXX     : Utils.XXX_XXX         := Utils.XXX_XXX_XXX;
       --  Optional
       Notes              : Description_String    := "";
       Dependencies       : Release_Dependencies  := No_Dependencies;
       Properties         : Release_Properties    := No_Properties;
       Private_Properties : Release_Properties    := No_Properties;
       Available_When     : Release_Requisites    := No_Requisites)
      return Release;
   --  Properties are generally interesting to the user
   --  Private_Properties are only interesting to alr

   ---------------------
   --  BASIC QUERIES  --
   ---------------------

   function Exists (Project : Alire.Project;
                    Version : Semantic_Versioning.Version)
                    return Boolean;

   function Find (Project : Alire.Project;
                  Version : Semantic_Versioning.Version) return Release;

private

   type Catalog_Entry (Name_Len, Descr_Len : Natural) is
   tagged record
      Project      : Alire.Project (1 .. Name_Len);
      Description  : Description_String (1 .. Descr_Len);
   end record;

end Alire.Index;
