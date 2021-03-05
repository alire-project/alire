with Alire.Conditional_Trees;
with Alire.Dependencies.Containers;
with Alire.Interfaces;
with Alire.Properties;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;

with Semantic_Versioning.Extended;

with TOML;

package Alire.Conditional with Preelaborate is

   ------------------
   -- Dependencies --
   ------------------

   package For_Dependencies is new Conditional_Trees (Dependencies.Dependency,
                                                      Dependencies.TTY_Image);
   subtype Dependencies is For_Dependencies.Tree;

   subtype Platform_Dependencies is Conditional.Dependencies
     with Dynamic_Predicate => Platform_Dependencies.Is_Unconditional;
   --  A plain tree without conditions (but might have OR nodes)

   subtype Forbidden_Dependencies is Platform_Dependencies
     with Dynamic_Predicate => not Forbidden_Dependencies.Contains_ORs;
   --  A plain tree without conditions or alternatives

   function New_Dependency
     (Name    : Crate_Name;
      Version : Semantic_Versioning.Version)
      return Dependencies;
   --  Dependency on a exact version

   function New_Dependency
     (Name     : Crate_Name;
      Versions : Semantic_Versioning.Extended.Version_Set)
      return Dependencies;
   --  Dependency on a version set

   function New_Dependency (Dep : Alire.Dependencies.Dependency)
                            return Conditional.Dependencies
                            renames For_Dependencies.New_Value;
   --  Convert a plain dependency into a tree containing a single leaf

   function Enumerate is new Conditional.For_Dependencies.Enumerate
     (Alire.Dependencies.Containers.List,
      Alire.Dependencies.Containers.Append);

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

   --------------------
   --  Availability  --
   --------------------

   --  We reuse the conditional trees for availability. This was not possible
   --  in the general Ada index, but it is now with the more limited case
   --  expressions. This allows removing the separate hierarchy of code
   --  that was formerly used only for availability.

   type Available is
     new Interfaces.Classificable
     and Interfaces.Tomifiable
     and Interfaces.Yamlable
   with record
      Is_Available : Boolean;
   end record;
   --  A wrapper on boolean to be able to store it in a conditional tree

   function Image (This : Available) return String;

   overriding
   function Key (This : Available) return String;

   overriding
   function To_TOML (This : Available) return TOML.TOML_Value;

   overriding
   function To_YAML (This : Available) return String;

   package For_Available is new Conditional_Trees (Available, Image);

   type Availability is new For_Available.Tree with null record;
   --  This is the actual type that encapsulates an expression tree

   function Available_From_TOML (From : TOML_Adapters.Key_Queue)
                                 return For_Available.Tree;
   --  Expects a single table "available = true/false"

   function Is_Available (This : Availability;
                          Env  : Alire.Properties.Vector)
                          return Boolean;
   --  Evaluate availability in an environment. In adition to resolving the
   --  tree for the environment, we then need to traverse the tree evaluating
   --  the boolean expressions to arrive to a final boolean value. (Formerly
   --  done via Boolean_Trees).

   function Available_Default return Availability
   is (New_Value (Available'(Is_Available => True)));
   --  Availability default is True unless an expression is given

private

   function New_Dependency
     (Name    : Crate_Name;
      Version : Semantic_Versioning.Version)
      return Dependencies is
     (For_Dependencies.New_Value
        (Alire.Dependencies.New_Dependency (Name, Version)));

   function New_Dependency
     (Name     : Crate_Name;
      Versions : Semantic_Versioning.Extended.Version_Set)
      return Dependencies is
     (For_Dependencies.New_Value
        (Alire.Dependencies.New_Dependency (Name, Versions)));

   function New_Property (Property : Alire.Properties.Property'Class)
                          return Properties is
     (For_Properties.New_Value (Property));

   function Image (This : Available) return String
   is (if This.Is_Available then "True" else "False");

   overriding
   function Key (This : Available) return String is (TOML_Keys.Available);

   overriding
   function To_TOML (This : Available) return TOML.TOML_Value
   is (TOML.Create_Boolean (This.Is_Available));

   overriding
   function To_YAML (This : Available) return String
   is (This.Key & ": " & This.Image);

end Alire.Conditional;
