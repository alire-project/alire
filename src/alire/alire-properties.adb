package body Alire.Properties is

   -------------
   -- To_TOML --
   -------------

   function To_TOML (V : Vector) return TOML.TOML_Value is
      use TOML;
   begin
      return TV : constant TOML_Value := Create_Array do
         for Prop of V loop
            Append (TV, Prop.To_TOML);
         end loop;
      end return;
   end To_TOML;

end Alire.Properties;
