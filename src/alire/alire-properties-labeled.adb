with Alire.Utils.Did_You_Mean;

package body Alire.Properties.Labeled is

   ------------
   -- Filter --
   ------------

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

   ------------
   -- Filter --
   ------------

   function Filter (PV   : Conditional.Properties;
                    Name : Labels) return Vector is
      (Filter (Conditional.Enumerate (PV), Name));

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
            if Labeled.Key (L) = K then
               return L;
            end if;
         end loop;

         declare
            Possible_Values : AAA.Strings.Vector;
         begin
            for L in Labels loop
               Possible_Values.Append (Labeled.Key (L));
            end loop;

            From.Checked_Error ("Key is not a valid property: " & K & "'" &
                                  Utils.Did_You_Mean.Suggestion
                                  (K, Possible_Values));
         end;
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
            for I in 1 .. Val.Length loop

               if Val.Item (I).Kind /= TOML_String then
                  raise Checked_Error with "Expected String value for " & Key;
               end if;

               declare
                  L : constant Label := New_Label
                    (Key_To_Label (Key),
                     Val.Item (I).As_String);
                  use all type Conditional.Properties;
               begin
                  if Cardinality (L.Name) = Unique and then I > 1 then
                     raise Checked_Error with
                       "Expected single value for " & Key;
                  end if;

                  --  Label-specific validation:
                  L.Validate (From);

                  --  Labeled property is valid and added to the release props.
                  Props := Props and
                    Conditional.For_Properties.New_Value (L);
               end;
            end loop;
         end;
      end return;
   exception
      when Checked_Error =>
         raise; -- Let the more informative error raise
      when E : others =>
         Log_Exception (E);
         From.Checked_Error ("Cannot read valid property from " & Key);
   end From_TOML;

   --------------
   -- Validate --
   --------------

   procedure Validate (L    : Label;
                       From : TOML_Adapters.Key_Queue) is
   begin
      case L.Name is
         when Description =>
            if L.Value'Length > Max_Description_Length then
               From.Checked_Error
                 ("Description string is too long (must be no more than"
                    & Max_Description_Length'Img & ")");
            end if;

         when Maintainer =>
            if not Utils.Could_Be_An_Email (L.Value, With_Name => True) then
               From.Checked_Error
                 ("Maintainers must have a valid email, but got: "
                  & L.Value);
            end if;

         when Maintainers_Logins =>
            if not Utils.Is_Valid_GitHub_Username (L.Value) then
               From.Checked_Error
                 ("maintainers-logins must be a valid GitHub login, but got: "
                  & L.Value);
            end if;

         when Tag =>
            if L.Value'Length = 0 then
               From.Checked_Error ("Tag string is empty");
            end if;

            if L.Value'Length > Max_Tag_Length then
               From.Checked_Error
                 ("Tag string '" & L.Value
                 & "' is too long (must be no more than"
                 & Max_Tag_Length'Img  & " characters)");
            end if;

            if not Utils.Is_Valid_Tag (L.Value) then
               From.Checked_Error ("Tag string is not valid: "
                                   & TTY.Error (L.Value));
            end if;
         when others =>
            null;
      end case;
   end Validate;

end Alire.Properties.Labeled;
