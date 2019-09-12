package body Alire.Requisites.Booleans is

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Tree
   is
      Value      : TOML.TOML_Value;
      Unused_Key : constant String := From.Pop (Value);
   begin
      --  Should never fail if the caller has properly constructed the adapter:
      return New_Requisite (Value.As_Boolean);
   end From_TOML;

end Alire.Requisites.Booleans;
