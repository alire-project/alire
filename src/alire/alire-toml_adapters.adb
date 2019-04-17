package body Alire.TOML_Adapters is

   --------------
   -- To_Array --
   --------------

   function To_Array (V : TOML.TOML_Value) return TOML.TOML_Value is
      use TOML;
   begin
      if V.Kind = TOML_Array then
         return V;
      else
         declare
            Arr : constant TOML_Value := Create_Array (V.Kind);
         begin
            Arr.Append (V);
            return Arr;
         end;
      end if;
   end To_Array;

end Alire.TOML_Adapters;
