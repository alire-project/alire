package body Alire.Properties.Bool is

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties
   is
      Value : TOML.TOML_Value;
      Key   : constant String := From.Pop (Value);

      ------------------
      -- Key_To_Label --
      ------------------

      function Key_To_Label (K : String) return Labels is
      begin
         --  TODO: instead of this inefficient O(n) lookup, have a map.
         for L in Labels loop
            if Bool.Key (L) = K then
               return L;
            end if;
         end loop;
         From.Checked_Error ("Key is not a valid boolean property: " & K);
      end Key_To_Label;

      --  For conditional loading, we use specific conditional loaders that
      --  only recognize the property being loaded:

   begin
      return Props : Conditional.Properties do
         declare
            Val : constant TOML.TOML_Value := TOML_Adapters.To_Array (Value);
            --  We process the same way single values and arrays of values
            --  (since they get converted into individual properties).
         begin
            if Val.Length > 1 then
               raise Checked_Error with
                 "Expected single value for " & Key;
            end if;

            if Val.Item (1).Kind /= TOML_Boolean then
               raise Checked_Error with "Expected boolean value for " & Key;
            end if;

            declare
               L : constant Property := New_Property
                 (Key_To_Label (Key),
                  Val.Item (1).As_Boolean);
               use all type Conditional.Properties;
            begin
               --  Labeled property is valid and added to the release props.
               Props := Props and
                 Conditional.For_Properties.New_Value (L);
            end;
         end;
      end return;
   exception
      when E : others =>
         Log_Exception (E);
         raise Checked_Error with "Cannot read valid property from " & Key;
   end From_TOML;

end Alire.Properties.Bool;
