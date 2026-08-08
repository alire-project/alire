with Ada.Containers.Indefinite_Ordered_Maps;

with AAA.Strings;

with Alire.Interfaces;
with Alire.TOML_Adapters;

with TOML;

package Alire.Crate_Features is

   type Selection is record
      Requested        : AAA.Strings.Set;
      Default_Features : Boolean := True;
   end record;

   Default_Selection : constant Selection :=
     (Requested => AAA.Strings.Empty_Set, Default_Features => True);

   function Selection_From_TOML
     (From : TOML_Adapters.Key_Queue) return Selection;

   function To_TOML (This : Selection) return TOML.TOML_Value;

   procedure Configure (This : Selection);
   --  Set an explicit command-line selection for this process.

   procedure Adopt (This : Selection);
   --  Adopt a lockfile selection unless Configure has already supplied an
   --  explicit command-line selection.

   function Current return Selection;

   type Definitions is new Interfaces.Tomifiable with private;

   function From_TOML
     (From : TOML_Adapters.Key_Queue) return Definitions;

   overriding
   function To_TOML (This : Definitions) return TOML.TOML_Value;

   function Is_Empty (This : Definitions) return Boolean;

   function Contains (This : Definitions; Name : String) return Boolean;

   function Members
     (This : Definitions; Name : String) return AAA.Strings.Set
     with Pre => This.Contains (Name);

   function Names (This : Definitions) return AAA.Strings.Set;

   function Identifier (Name : String) return String;
   --  Convert a feature name into the suffix used by generated Ada, C, and
   --  GPR configuration constants.

   procedure Validate (This         : Definitions;
                       Dependencies : AAA.Strings.Set;
                       Optional     : AAA.Strings.Set);
   --  Validate local references and dependency forwarding after the release's
   --  complete dependency tree has been loaded.

private

   Current_Selection : Selection := Default_Selection;
   Explicit_Selection : Boolean := False;

   package Definition_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, AAA.Strings.Set, "<", AAA.Strings."=");

   type Definitions is new Interfaces.Tomifiable with record
      Map : Definition_Maps.Map;
   end record;

end Alire.Crate_Features;
