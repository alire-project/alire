with Alire.Conditional_Trees;
with Alire.Dependencies;
with Alire.Properties;
with Alire.TOML_Adapters;

with Semantic_Versioning.Extended;

package Alire.Conditional with Preelaborate is

   ------------------
   -- Dependencies --
   ------------------

   package For_Dependencies is new Conditional_Trees (Dependencies.Dependency,
                                                      Dependencies.Image);
   subtype Dependencies is For_Dependencies.Tree;

   subtype Platform_Dependencies is Conditional.Dependencies
     with Dynamic_Predicate => Platform_Dependencies.Is_Unconditional;
   --  A plain tree without conditions (but might have OR nodes)

   subtype Forbidden_Dependencies is Platform_Dependencies
     with Dynamic_Predicate => not Forbidden_Dependencies.Contains_ORs;
   --  A plain tree without conditions or alternatives

   function New_Dependency
     (Name     : Crate_Name;
      Versions : Semantic_Versioning.Extended.Version_Set)
      return Dependencies;

   function No_Dependencies return Dependencies is (For_Dependencies.Empty);

   function Deps_From_TOML (From : TOML_Adapters.Key_Queue)
                            return Dependencies;
   --  Expects a wrapped table of crate = version_set pairs:
   --  depends-on = { crate = version [...] }

   ----------------
   -- Properties --
   ----------------

   package For_Properties
   is new Conditional_Trees (Properties.Property'Class,
                             Properties.Image_Classwide);
   subtype Properties is For_Properties.Tree;

   subtype Unconditional_Properties is Conditional.Properties with
     Dynamic_Predicate => Unconditional_Properties.Is_Unconditional;
   --  A plain list of properties without dynamic expressions.

   function New_Property (Property : Alire.Properties.Property'Class)
                          return Properties;

   function Enumerate is new Conditional.For_Properties.Enumerate
     (Alire.Properties.Vector, Alire.Properties.Append);

   function No_Properties return Properties is (For_Properties.Empty);

   type Property_Loader is access
     function (From : TOML_Adapters.Key_Queue)
               return Conditional.Properties;
   --  Function provided by each concrete Property class for TOML loading.
   --  From is always a table "prop-name = whatever".
   --  These may raise Checked_Error.

private

   function New_Dependency
     (Name     : Crate_Name;
      Versions : Semantic_Versioning.Extended.Version_Set)
      return Dependencies is
     (For_Dependencies.New_Value
        (Alire.Dependencies.New_Dependency (Name, Versions)));

   function New_Property (Property : Alire.Properties.Property'Class)
                          return Properties is
     (For_Properties.New_Value (Property));

end Alire.Conditional;
