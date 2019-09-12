package body Alire.Properties.Licenses is

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties
   is
      use TOML;
      use all type Conditional.Properties;
      Props : Conditional.Properties;
      Value : constant TOML.TOML_Value := From.Pop;
   begin
      if Value.Kind /= TOML_Array then
         raise Checked_Error with "license expects an array";
      else
         for I in 1 .. Value.Length loop
            if Value.Item (I).Kind = TOML_String then
               Props := Props and
                 Conditional.For_Properties.New_Value
                   (New_License (Value.Item (I).As_String));
            else
               raise Checked_Error with "licenses must be strings";
            end if;
         end loop;
      end if;

      return Props;
   exception
      when E : Checked_Error => -- May happen on unknown non-custom license.
         From.Checked_Error (Errors.Get (E));
         --  Re-raise with full context of From.
   end From_TOML;

end Alire.Properties.Licenses;
