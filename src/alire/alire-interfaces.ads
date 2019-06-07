with Alire.Utils;

with TOML;

package Alire.Interfaces with Preelaborate is

   -------------------
   -- Classificable --
   -------------------

   type Classificable is limited interface;
   --  Used to classify properties/dependencies when exporting to TOML

   function Key (This : Classificable) return String is abstract;

   ----------------
   -- Codifiable --
   ----------------

   type Codifiable is limited interface;

   function To_Code (This : Codifiable) return Utils.String_Vector is abstract;

   ----------------
   -- Imaginable --
   ----------------

   type Imaginable is limited interface;

   function Image (This : Imaginable) return String is abstract;

   ----------------
   -- Tomifiable --
   ----------------

   type Tomifiable is limited interface;
   --  Implemented by types that appear in the index
   --  This can be recursive (trees, arrays...)

   function To_TOML (This : Tomifiable) return TOML.TOML_Value is abstract;

end Alire.Interfaces;
