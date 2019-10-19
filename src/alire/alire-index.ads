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
with Alire.Versions;

with Semantic_Versioning;

package Alire.Index is

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
