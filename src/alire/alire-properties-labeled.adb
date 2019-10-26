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
         From.Checked_Error ("Key is not a valid property: " & K);
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

                  Props := Props and
                    Conditional.For_Properties.New_Value (L);
               end;
            end loop;
         end;
      end return;
   exception
      when E : others =>
         Log_Exception (E);
         raise Checked_Error with "Cannot read valid property from " & Key;
   end From_TOML;

   ------------------------
   -- Loader_During_Case --
   ------------------------

   generic
      Key : String;
   function Loader_During_Case (From : TOML_Adapters.Key_Queue)
                                return Conditional.Properties;
   --  We use this wrapper to ensure that the labels and values match after
   --  traversing a dynamic case expression.

   function Loader_During_Case (From : TOML_Adapters.Key_Queue)
                                return Conditional.Properties
   is
      --  In the case of labeled properties, the key will be the case entry,
      --  that we don't need.
      Key_Got : constant String := +From.Unwrap.Keys (1);
   begin
      if Key = Key_Got then
         return From_TOML (From);
      else
         From.Checked_Error ("expected " & Key & " but got " & Key_Got);
      end if;
   end Loader_During_Case;

   function From_TOML_Executable_Cases_Internal is
     new Loader_During_Case (TOML_Keys.Executable);

   function From_TOML_Executable_Cases
     (From : TOML_Adapters.Key_Queue)
      return Conditional.Properties
      renames From_TOML_Executable_Cases_Internal;

   function From_TOML_Project_File_Cases_Internal is
     new Loader_During_Case (TOML_Keys.Project_File);

   function From_TOML_Project_File_Cases
     (From : TOML_Adapters.Key_Queue)
      return Conditional.Properties
      renames From_TOML_Project_File_Cases_Internal;

end Alire.Properties.Labeled;
