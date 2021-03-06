with Alire.TOML_Adapters;

generic
   Key  : String; -- The TOML key used in the index for this enum
   Name : String := TOML_Adapters.Adafy (Key); -- An Ada-like name, overridable
   type Ada_Enum is (<>);
package Alire.Expressions.Enums with Elaborate_Body is

   --  Declares a pseudotype that encapsulates an actual Ada enumeration. By
   --  instancing this package the type becomes recognizable by the index
   --  loading functions.

end Alire.Expressions.Enums;
