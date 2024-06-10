with AAA.Strings;

package body Alire.Expressions.Enums is

   type Enum_Values is new Expressions.Values with record
      Values : AAA.Strings.Set;
   end record;

   overriding
   function Is_Valid (This : Enum_Values; Image : String) return Boolean
   is (This.Values.Contains (TOML_Adapters.Tomify (Image)));

   overriding
   function Possible_Values (This : Enum_Values) return AAA.Strings.Vector is
      Result : AAA.Strings.Vector;
   begin
      Result.Prepend (This.Values);
      return Result;
   end Possible_Values;

begin
   declare
      Values : Enum_Values;
   begin
      for Enum_Value in Ada_Enum'Range loop
         Values.Values.Insert (TOML_Adapters.Tomify (Enum_Value'Image));
      end loop;

      Trace.Debug ("Registering variable for expressions: " & Key);
      Expressions.Register (Var_Key    => Key,
                            Var_Name   => Name,
                            Var_Values => Values);
   end;
end Alire.Expressions.Enums;
