package body Alire.Conditional is

   -------------------------
   -- Available_From_TOML --
   -------------------------

   function Available_From_TOML (From : TOML_Adapters.Key_Queue)
                                 return For_Available.Tree
   is
   begin
      if From.Unwrap.Is_Null or else From.Unwrap.Keys'Length = 0 then
         return For_Available.Empty;
      else
         return For_Available.New_Value
           (Available'
              (Is_Available =>
               From.Checked_Pop
                 (TOML_Keys.Available,
                  TOML.TOML_Boolean).As_Boolean));
      end if;
   end Available_From_TOML;

   --------------------
   -- Deps_From_TOML --
   --------------------

   function Deps_From_TOML (From : TOML_Adapters.Key_Queue) return Dependencies
   is
      use type Dependencies;
   begin
      return Result : Dependencies do
         for Pair of From.Pop.Iterate_On_Table loop
            Result := Result and
              Alire.Dependencies.From_TOML (+Pair.Key,
                                            Pair.Value);
         end loop;
      end return;
   end Deps_From_TOML;

end Alire.Conditional;
