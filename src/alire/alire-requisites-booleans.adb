package body Alire.Requisites.Booleans is

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Tree
   is
      Value      : TOML.TOML_Value;
      Unused_Key : constant String := From.Pop (Value);
   begin
      --  Should never fail if the caller has properly constructed the adapter:
      if Value.Kind in TOML.TOML_Boolean then
         return New_Requisite (Value.As_Boolean);
      else
         From.Checked_Error ("expected a Boolean value, but got a "
                             & Value.Kind'Img);
      end if;
   end From_TOML;

end Alire.Requisites.Booleans;
