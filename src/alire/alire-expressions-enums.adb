with Alire.TOML_Adapters;
with Alire.Utils;

package body Alire.Expressions.Enums is

   type Enum_Values is new Expressions.Values with record
      Values : Utils.String_Set;
   end record;

   overriding
   function Is_Valid (This : Enum_Values; Image : String) return Boolean
   is (This.Values.Contains (TOML_Adapters.Tomify (Image)));

begin
   declare
      Values : Enum_Values;
   begin
      for Enum_Value in Ada_Enum'Range loop
         Values.Values.Insert (TOML_Adapters.Tomify (Enum_Value'Image));
      end loop;

      Expressions.Types.Insert (Name, Values);
   end;
end Alire.Expressions.Enums;
