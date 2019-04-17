package body Alire.Properties is

   ------------
   -- Filter --
   ------------

   function Filter (V : Vector; Ancestor : Ada.Tags.Tag) return Vector is
      Result : Vector := No_Properties;
   begin
      for Prop of V loop
         if Ada.Tags.Is_Descendant_At_Same_Level (Prop'Tag, Ancestor) then
            Result.Append (Prop);
         end if;
      end loop;

      return Result;
   end Filter;

   -------------
   -- To_TOML --
   -------------

   function To_TOML (V : Vector) return TOML.TOML_Value is
      use TOML;
   begin
      if V.Is_Empty then
         return Create_Array (TOML_String); -- Ensure typed
      else
         return TV : constant TOML_Value := Create_Array do
            for Prop of V loop
               Append (TV, Prop.To_TOML);
            end loop;
         end return;
      end if;
   end To_TOML;

end Alire.Properties;
