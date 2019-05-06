private with Alire.Utils;

with TOML; use all type TOML.Any_Value_Kind;

package Alire.TOML_Adapters with Preelaborate is

   --  Helpers to create TOML values with ease

   function "+" (S : String) return TOML.TOML_Value is
      (TOML.Create_String (S));

   function To_Array (V : TOML.TOML_Value) return TOML.TOML_Value with
     Pre  => V.Kind in TOML.Atom_Value_Kind or V.Kind = TOML.TOML_Array,
     Post => To_Array'Result.Kind = TOML.TOML_Array;
   --  Take an atom value and return an array of a single element
   --  If already an array, do nothing

   generic
      type Enum is (<>);
   function Tomify (E : Enum) return TOML.TOML_Value;
   --  Simple tomifier for when the image is enough
   --  The resulting string is lowercase and with - instead of _
   --  E.g: Post_Fetch becomes post-fetch

private

   function Tomify (E : Enum) return TOML.TOML_Value is
     (TOML.Create_String
        (Utils.Replace
             (Utils.To_Lower_Case (E'Img),
              Match => "_",
              Subst => "-")));

end Alire.TOML_Adapters;
