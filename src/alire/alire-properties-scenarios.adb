package body Alire.Properties.Scenarios is

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties
   is
      use Conditional.For_Properties;
      use TOML;

      Value : TOML.TOML_Value;
      Key   : constant String := From.Pop (Value);

      --  This function both processes scenario declarations and the setting of
      --  scenario variables, which are stored in the same type of property.
      --  The two following functions are devoted respectively to each case.

      -----------------------
      -- Process_Externals --
      -----------------------

      function Process_Externals return Conditional.Properties is
         Table : constant TOML_Adapters.Key_Queue := From.Descend
           (Value, TOML_Keys.GPR_Ext);
      begin
         return Props : Conditional.Properties do
            loop
               declare
                  Val : TOML.TOML_Value;
                  Key : constant String := Table.Pop (Val);
               begin
                  exit when Key = "";

                  if Val.Kind = TOML_String then
                     if Val.As_String = "" then
                        Props := Props and
                          (New_Property (GPR.Free_Variable (Key)));
                     else
                        Table.Checked_Error
                          ("free scenario variable must be given as """"");
                        return;
                     end if;
                  elsif Val.Kind = TOML_Array then
                     if Val.Length < 2 then
                        Table.Checked_Error
                          ("At least two values required in scenario");
                     end if;
                     if Val.Item_Kind = TOML_String then
                        declare
                           use GPR;
                           Values : GPR.Value_Vector;
                        begin
                           for I in 1 .. Val.Length loop
                              Values := Values or Val.Item (I).As_String;
                           end loop;
                           Props := Props and New_Property
                             (GPR.Enum_Variable (Key, Values));
                        end;
                     else
                        Table.Checked_Error
                          ("scenario values must be a string array");
                     end if;
                  end if;
               end;
            end loop;

            if Props.Is_Empty then
               Table.Checked_Error ("empty table");
            end if;
         end return;
      end Process_Externals;

      ---------------------------
      -- Process_Set_Externals --
      ---------------------------

      function Process_Set_Externals return Conditional.Properties is
         Table : constant TOML_Adapters.Key_Queue := From.Descend
           (Value, TOML_Keys.GPR_Set_Ext);
      begin
         return Props : Conditional.Properties do
            loop
               declare
                  Val : TOML.TOML_Value;
                  Key : constant String := Table.Pop (Val);
               begin
                  exit when Key = "";

                  if Val.Kind /= TOML_String then
                     Table.Checked_Error
                       ("external variables must be given as "
                        & "'var = string' pairs");
                     return;
                  end if;

                  Props := Props and New_Property
                    (GPR.External_Value (Key, Val.As_String));
               end;
            end loop;

            if Props.Is_Empty then
               Table.Checked_Error ("scenario sets no externals");
            end if;
         end return;
      end Process_Set_Externals;

   begin
      if Value.Kind /= TOML_Table then
         From.Checked_Error ("scenarios require a table");
      end if;

      if Key = TOML_Keys.GPR_Ext then
         return Process_Externals;
      else
         return Process_Set_Externals;
      end if;
   end From_TOML;

   ---------------------
   -- From_TOML_Cases --
   ---------------------

   function From_TOML_Cases (From : TOML_Adapters.Key_Queue)
                             return Conditional.Properties is
     (if +From.Unwrap.Keys (1)  = TOML_Keys.GPR_Set_Ext
      then From_TOML (From)
      else raise Checked_Error with
        From.Message ("scenario expressions can only set externals"));

   -------------
   -- To_TOML --
   -------------

   overriding function To_TOML (V : Property) return TOML.TOML_Value is
      Table : constant TOML.TOML_Value := TOML.Create_Table;
      use all type GPR.Variable_Kinds;
      use TOML_Adapters;
   begin
      case V.Var.Element.Kind is
         when Enumeration =>
            declare
               Arr : constant TOML.TOML_Value :=
                 TOML.Create_Array (TOML.TOML_String);
            begin
               for Val of V.Var.Element.Values loop
                  Arr.Append (+Val);
               end loop;

               Table.Set (V.Var.Element.Name, Arr);
            end;
         when Free_String =>
            Table.Set (V.Var.Element.Name, +"");
         when External =>
            Table.Set (V.Var.Element.Name, +V.Var.Element.External_Value);
      end case;

      return Table;
   end To_TOML;

end Alire.Properties.Scenarios;
