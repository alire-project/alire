with Alire.TOML_Adapters;

with TOML;

package Alire.Interfaces with Preelaborate is

   ------------------
   -- Classifiable --
   ------------------

   type Classifiable is limited interface;
   --  Used to classify properties/dependencies when exporting to TOML

   function Key (This : Classifiable) return String is abstract;

   ----------------
   -- Imaginable --
   ----------------

   type Imaginable is limited interface;

   function Image (This : Imaginable) return String is abstract;

   ---------------
   -- Colorable --
   ---------------

   type Colorable is limited interface;
   --  Types that can be displayed with ANSI colors/formatting

   function TTY_Image (This : Colorable) return String is abstract;

   ----------------
   -- Tomifiable --
   ----------------

   type Tomifiable is limited interface;
   --  Implemented by types that appear in the index
   --  This can be recursive (trees, arrays...)

   function To_TOML (This : Tomifiable) return TOML.TOML_Value is abstract;

   ------------------
   -- Detomifiable --
   ------------------

   type Detomifiable is limited interface;
   --  Implemented by types that can be loaded from a TOML crate file;
   --  in particular by complex objects stored as tables.

   function From_TOML (This : in out Detomifiable;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome is abstract;
   --  To allow partial load this uses an in out object.

   --------------
   -- Yamlable --
   --------------

   type Yamlable is limited interface;

   function To_YAML (This : Yamlable) return String is abstract;
   --  Return a YAML text representation of the object

end Alire.Interfaces;
