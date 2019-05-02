package body Alire.Properties.Labeled is

   function Filter (LV : Vector; Name : Labels) return Vector is
   begin
      return Result : Vector do
         for L of LV loop
            if L in Label and then
              Label'Class (L).Name = Name
            then
               Result.Append (L);
            end if;
         end loop;
      end return;
   end Filter;

   function To_TOML_Array (LV   : Vector;
                           Name : Labels)
                           return TOML.TOML_Value
   is
      use TOML;
      Values : constant TOML_Value := Create_Array;
   begin
      for V of Filter (LV, Name) loop
         Values.Append (Create_String (Labeled.Label'Class (V).Value));
      end loop;

      return Values;
   end To_TOML_Array;

end Alire.Properties.Labeled;
