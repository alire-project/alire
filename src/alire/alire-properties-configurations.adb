with TOML; use TOML;

with Alire.Utils.YAML;

with Ada.Characters.Handling;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Alire.Properties.Configurations is

   -------------
   -- To_Type --
   -------------

   function To_Type (Str : String) return Config_Type_Kind is
      Lower : constant String := Ada.Characters.Handling.To_Lower (Str);
   begin
      if Lower = "real" then
         return Real;
      elsif Lower = "integer" then
         return Int;
      elsif Lower = "string" then
         return Configurations.Str;
      elsif Lower = "enum" then
         return Enum;
      elsif Lower = "boolean" then
         return Bool;
      end if;

      Raise_Checked_Error
        ("Invalid configuration type '" & Str &
           "', must be (real, integer, string, enum or boolean)");
   end To_Type;

   ---------------
   -- To_String --
   ---------------

   function To_String (Str_Array        : TOML_Value;
                       Wrap_With_Quotes : Boolean := False)
                       return String
   is
      Res : Unbounded_String;
      First : Boolean := True;
   begin
      if Str_Array.Kind /= TOML_Array
        and then
         Str_Array.Item_Kind /= TOML_String
      then
         raise Program_Error with "Invalid TOML kind for enum values";
      end if;

      for Index in 1 .. Str_Array.Length loop
         declare
            Val : constant String := Str_Array.Item (Index).As_String;
            Val_Str : constant Unbounded_String := +(if Wrap_With_Quotes
                                                     then """" & Val & """"
                                                     else Val);
         begin
            if First then
               Res := Res & Val_Str;
               First := False;
            else
               Res := Res & ", " & Val_Str;
            end if;
         end;
      end loop;

      return +Res;
   end To_String;

   -----------
   -- Image --
   -----------

   function Image (This : Config_Integer) return String is
      Ret : constant String := This'Img;
   begin
      if Ret (Ret'First) = ' ' then
         return Ret (Ret'First + 1 .. Ret'Last);
      else
         return Ret;
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (This : Config_Real) return String is
   begin
      case This.Kind is
         when Regular =>
            declare
               Ret : constant String := This.Value'Img;
            begin
               if Ret (Ret'First) = ' ' then
                  return Ret (Ret'First + 1 .. Ret'Last);
               else
                  return Ret;
               end if;
            end;
         when NaN =>
            if This.Positive then
               return "+nan";
            else
               return "-nan";
            end if;
         when Infinity =>
            if This.Positive then
               return "+inf";
            else
               return "-inf";
            end if;
      end case;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Val : TOML_Value) return String is
   begin
      if Val = No_TOML_Value then
         Raise_Checked_Error
           ("Unexpected No_TOML_Value in conversion to String");
      end if;

      case Val.Kind is
         when TOML_String =>
            return Val.As_String;
         when TOML_Boolean =>
            return Val.As_Boolean'Img;
         when TOML_Float =>
            return Image (Val.As_Float);
         when TOML_Integer =>
            return Image (Val.As_Integer);

         when TOML_Array =>
            if Val.Item_Kind /= TOML_String then
               Raise_Checked_Error ("Unexpected kind '" & Val.Item_Kind'Img &
                                      "' in array conversion to String");
            else
               return To_String (Val);
            end if;
         when others =>
            Raise_Checked_Error ("Unexpected kind '" & Val.Kind'Img &
                                   "' in conversion to String");
      end case;
   end Image;

   --------------------
   -- Type_To_String --
   --------------------

   function Type_To_String (This : Config_Type_Definition) return String
   is ((case This.Kind is
           when Str => "String",
           when Bool => "Boolean",
           when Enum => "Enum (" & To_String (This.Values) & ")",
           when Real => "Real range " & Image (This.Real_First) & " .. " &
                                        Image (This.Real_Last),
           when Int  => "Integer range " & Image (This.Int_First) & " .. " &
                                           Image (This.Int_Last))
      );

   -----------
   -- Image --
   -----------

   overriding
   function Image (This : Config_Type_Definition) return String is
      Ret : Unbounded_String;
   begin
      Ret := "Config type: " & This.Name & " : " & Type_To_String (This) & "";

      if This.Default /= No_TOML_Value then
         Append (Ret, String'(" default: '" & Image (This.Default) & "'"));
      end if;

      return +Ret;
   end Image;

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (This : Config_Type_Definition) return TOML.TOML_Value is
      Table1 : constant TOML.TOML_Value := TOML.Create_Table;
      Table2 : constant TOML.TOML_Value := TOML.Create_Table;

   begin
      Table2.Set ("type", Create_String (case This.Kind is
                     when Str => "String",
                     when Enum => "Enum",
                     when Int  => "Integer",
                     when Bool => "Boolean",
                     when Real => "Real"));

      if This.Default /= TOML.No_TOML_Value then
         Table2.Set ("default", This.Default);
      end if;

      case This.Kind is
         when Enum =>
            Table2.Set ("values", This.Values);

         when Int =>
            Table2.Set ("first", Create_Integer (This.Int_First));
            Table2.Set ("last", Create_Integer (This.Int_Last));

         when Real =>
            Table2.Set ("first", Create_Float (This.Real_First));
            Table2.Set ("last", Create_Float (This.Real_Last));

         when Bool | Str => null;
      end case;

      Table1.Set (+This.Name, Table2);
      return Table1;
   end To_TOML;

   -------------
   -- To_YAML --
   -------------

   overriding
   function To_YAML (This : Config_Type_Definition) return String is
      Ret : Unbounded_String;
   begin
      Ret := "{name: '" & This.Name & "', type: '" &
        Type_To_String (This) & "'";

      if This.Default /= No_TOML_Value then
         Append (Ret, ", default: " &
                   Alire.Utils.YAML.YAML_Stringify (Image (This.Default)));
      end if;

      return +Ret & "}";
   end To_YAML;

   -----------
   -- Image --
   -----------

   overriding
   function Image (This : Config_Value_Assignment) return String is
      Ret : Unbounded_String;
      First : Boolean := True;
   begin
      Ret := "Config set: " & This.Crate & " {";

      for Elt of This.List loop
         if not First then
            Append (Ret, ", ");
         else
            First := False;
         end if;

         Append (Ret, +Elt.Name & " := " & Image (Elt.Value));
      end loop;

      Append (Ret, "}");
      return +Ret;
   end Image;

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (This : Config_Value_Assignment) return TOML.TOML_Value is
      Root   : constant TOML.TOML_Value := TOML.Create_Table;
      Assign : constant TOML.TOML_Value := TOML.Create_Table;

   begin

      for Elt of This.List loop
         Assign.Set (Elt.Name, Elt.Value);
      end loop;
      Root.Set (+This.Crate, Assign);

      return Root;
   end To_TOML;

   -------------
   -- To_YAML --
   -------------

   overriding
   function To_YAML (This : Config_Value_Assignment) return String is
      Ret : Unbounded_String;
      First : Boolean := True;
   begin
      Ret := "{crate: '" & This.Crate & "', settings: [";

      for Elt of This.List loop
         if not First then
            Append (Ret, ", " & ASCII.LF);
         else
            First := False;
         end if;

         Append (Ret, "{name: '" & Elt.Name & "', value: " &
                   Alire.Utils.YAML.YAML_Stringify (Image (Elt.Value)) &
                "}");
      end loop;

      Append (Ret, "]}");
      return +Ret;
   end To_YAML;

   -----------
   -- Valid --
   -----------

   function Valid (This : Config_Type_Definition;
                   Val  : TOML.TOML_Value)
                   return Boolean
   is
   begin

      if Val = No_TOML_Value then
         return False;
      end if;

      --  Validate the TOML type
      case This.Kind is
         when Str | Enum =>
            if Val.Kind /= TOML_String then
               return False;
            end if;
         when Bool =>
            if Val.Kind /= TOML_Boolean then
               return False;
            end if;
         when Real =>
            if Val.Kind /= TOML_Float then
               return False;
            end if;
         when Int =>
            if Val.Kind /= TOML_Integer then
               return False;
            end if;
      end case;

      case This.Kind is
         when Str | Bool =>
            --  Nothing else to check
            return True;
         when Enum =>
            declare
               Str : constant String := Val.As_String;
            begin
               for Index in 1 .. This.Values.Length loop
                  if This.Values.Item (Index).As_String = Str then
                     return True;
                  end if;
               end loop;
               return False;
            end;

         when Real =>

            declare
               F : constant Config_Real := Val.As_Float;
               First : Config_Real renames This.Real_First;
               Last  : Config_Real renames This.Real_Last;
            begin

               --  We don't accept infinity or Nan here
               if F.Kind /= Regular then
                  return False;
               end if;

               return not (
                  --  Lower bound
                  (First.Kind = Infinity and then First.Positive)
                 or else
                  (First.Kind /= Infinity and then F.Value < First.Value)
                 or else
                  --  Upper bound
                  (Last.Kind = Infinity and then not Last.Positive)
                 or else
                  (Last.Kind /= Infinity and then F.Value > Last.Value)
                 );
            end;
         when Int =>
            declare
               I : constant Config_Integer := Val.As_Integer;
            begin
               return I >= This.Int_First and then I <= This.Int_Last;
            end;
      end case;
   exception
      when others =>
         return False;
   end Valid;

   ----------
   -- Name --
   ----------

   function Name (This : Config_Type_Definition) return String
   is (+This.Name);

   ------------------------
   -- To_Ada_Declaration --
   ------------------------

   function To_Ada_Declaration (This : Config_Type_Definition;
                                Value : TOML.TOML_Value)
                                return String
   is
      use ASCII;
      Name : constant String := +This.Name;
      Indent : constant String := "   ";
   begin
      case This.Kind is

         when Str =>
            return Indent & Name & " : constant String := """ &
              Value.As_String & """;";

         when Bool =>
            return Indent & Name & " : constant Boolean := " &
              (if Value.As_Boolean then "True" else "False") & ";";

         when Enum =>
            return Indent & "type " & Name & "_Kind is (" &
              To_String (This.Values) & ");" & LF &
              Indent & Name & " : constant " & Name & "_Kind := " &
              Value.As_String & ";";

         when Real =>

            --  For Real and Int we do not declare a ranged type such as:
            --
            --  type Test_Type is range 0 .. 100;
            --  Test : constant Test_Type := 50;
            --
            --  because this would limit the possibilities of users for things
            --  such as alignment size, etc. Instead we define named numbers
            --  (constant) for the First, Last and actual value of the
            --  variable:
            --
            --   Test_First : constant := 0;
            --   Test_Last : constant := 100
            --   Test : constant := 50;
            --
            --  User can then define their own types:
            --
            --   type Test_Type is range Config.Test_First .. Config.Test_Last
            --     with Size => 32;

            return Indent & Name & "_First : constant := " &
              Image (This.Real_First) & ";" & LF &
              Indent & Name & "_Last : constant := " &
              Image (This.Real_Last) & ";" & LF &
              Indent & Name & " : constant := " &
              Image (Value.As_Float) & ";";

         when Int =>

            return Indent & Name & "_First : constant := " &
            This.Int_First'Img & ";" & LF &
              Indent & Name & "_Last : constant := " &
            This.Int_Last'Img & ";" & LF &
              Indent & Name & " : constant := " &
              Value.As_Integer'Img & ";";

      end case;
   end To_Ada_Declaration;

   ------------------------
   -- To_GPR_Declaration --
   ------------------------

   function To_GPR_Declaration (This : Config_Type_Definition;
                                Value : TOML.TOML_Value)
                                return String
   is
      use ASCII;
      Name : constant String := +This.Name;
      Indent : constant String := "   ";
   begin
      case This.Kind is

         when Str =>
            return Indent & Name & " := """ & Value.As_String & """;";

         when Bool =>
            return Indent & Name & " := """ &
            (if Value.As_Boolean then "True" else "False") & """;";

         when Enum =>
            return Indent & "type " & Name & "_Kind is (" &
              To_String (This.Values, Wrap_With_Quotes => True) & ");" & LF &
              Indent & Name & " : " & Name & "_Kind := """ & Value.As_String
              & """;";

         when Real =>

            return Indent & Name & "_First := """ &
              Image (This.Real_First) & """;" & LF &
              Indent & Name & "_Last := """ &
              Image (This.Real_Last) & """;" & LF &
              Indent & Name & " := """ &
              Image (Value.As_Float) & """;";

         when Int =>

            return Indent & Name & "_First := """ &
              Image (This.Int_First) & """;" & LF &
              Indent & Name & "_Last := """ &
              Image (This.Int_Last) & """;" & LF &
              Indent & Name & " := """ & Image (Value.As_Integer) & """;";

      end case;
   end To_GPR_Declaration;

   ----------------------
   -- To_C_Declaration --
   ----------------------

   function To_C_Declaration (This : Config_Type_Definition;
                              Value : TOML.TOML_Value)
                              return String
   is
      use ASCII;
      Name : constant String := +This.Name;

   begin
      case This.Kind is

         when Str =>
            return "#define " & Name & " """ & Value.As_String & """";

         when Bool =>
            return "#define " & Name & "_True 1" & LF &
              "#define " & Name & "_False 0" & LF &
              "#define " & Name & " " &
            (if Value.As_Boolean then "1" else "0");

         when Enum =>
            declare
               Ret : Unbounded_String;
               Count : Positive := 1;

               Value_Id : Positive := 1;
            begin
               for Index in 1 .. This.Values.Length loop
                  declare
                     Elt : constant String :=
                       This.Values.Item (Index).As_String;
                  begin
                     Ret := Ret & "#define " & Name & "_" & Elt  &
                       Count'Img & LF;

                     if Elt = Value.As_String then
                        Value_Id := Count;
                     end if;

                     Count := Count + 1;
                  end;
               end loop;

               return +Ret & LF &
                 "#define " & Name & " " & Value_Id'Img;
            end;

         when Real =>
            return
              "#define " & Name & "_First " & Image (This.Real_First) & LF &
              "#define " & Name & "_Last " & Image (This.Real_Last) & LF &
              "#define " & Name & " " & Image (Value.As_Float);

         when Int =>
            return
              "#define " & Name & "_First " & Image (This.Int_First) & LF &
              "#define " & Name & "_Last " & Image (This.Int_Last) & LF &
              "#define " & Name & " " & Image (Value.As_Integer);

      end case;
   end To_C_Declaration;

   ---------------------------
   -- Definitions_From_TOML --
   ---------------------------

   function Definitions_From_TOML (From : TOML_Adapters.Key_Queue)
                                   return Conditional.Properties
   is
      use Conditional.For_Properties;

      ----------------
      -- Create_One --
      ----------------

      function Create_One (Name : String;
                           Raw  : TOML.TOML_Value)
                           return Conditional.Properties
      is
         From    : constant TOML_Adapters.Key_Queue :=
           TOML_Adapters.From (Raw, TOML_Keys.Config_Vars & "." & Name);
         Typ     : TOML_Value;
         First   : TOML_Value;
         Last    : TOML_Value;

         Unused : Boolean;

      begin
         if Raw.Kind /= TOML_Table then
            From.Checked_Error ("variable definition must be a table");
         end if;

         if not From.Pop ("type", Typ) then
            From.Checked_Error ("'type' missing");
         end if;

         if Typ.Kind /= TOML_String then
            From.Checked_Error ("'type' must be string");
         end if;

         declare
            Type_Def : Config_Type_Definition (To_Type (Typ.As_String));
         begin
            Type_Def.Name := +Name;

            case Type_Def.Kind is

               when Int =>
                  if From.Pop ("first", First) then
                     if First.Kind /= TOML_Integer then
                        From.Checked_Error ("first must be interger");
                     end if;

                     Type_Def.Int_First := First.As_Integer;
                  else
                     Type_Def.Int_First := Config_Integer'First;
                  end if;

                  if From.Pop ("last", Last) then
                     if Last.Kind /= TOML_Integer then
                        From.Checked_Error ("'last' must be integer");
                     end if;

                     Type_Def.Int_Last := Last.As_Integer;
                  else
                     Type_Def.Int_Last := Config_Integer'Last;
                  end if;

               when Real =>
                  if From.Pop ("first", First) then
                     if First.Kind /= TOML_Float then
                        From.Checked_Error ("'first' must be float/real");
                     end if;

                     Type_Def.Real_First := First.As_Float;

                     if Type_Def.Real_First.Kind = TOML.NaN then
                        From.Checked_Error ("'first' cannot be NaN");
                     end if;
                  else
                     Type_Def.Real_First := (TOML.Infinity, Positive => False);
                  end if;

                  if From.Pop ("last", Last) then
                     if Last.Kind /= TOML_Float then
                        From.Checked_Error ("'last' must be float/real");
                     end if;

                     Type_Def.Real_Last := Last.As_Float;

                     if Type_Def.Real_Last.Kind = TOML.NaN then
                        From.Checked_Error ("'last' cannot be NaN");
                     end if;
                  else
                     Type_Def.Real_Last := (TOML.Infinity, Positive => True);
                  end if;

               when Enum =>
                  if From.Pop ("values", Type_Def.Values) then
                     if Type_Def.Values.Kind /= TOML_Array
                       or else
                        not Type_Def.Values.Item_Kind_Set
                       or else
                         Type_Def.Values.Item_Kind /= TOML_String
                     then
                        From.Checked_Error
                          ("'values' must be a not empty array of strings");
                     end if;
                  else
                     From.Checked_Error
                       ("missing 'values' for enumeration type");
                  end if;

               when others =>
                  null;
            end case;

            if From.Pop ("default", Type_Def.Default) then
               if not Valid (Type_Def, Type_Def.Default) then
                  From.Checked_Error ("invalid default value for " &
                                        Type_To_String (Type_Def));
               end if;
            else
               Type_Def.Default := No_TOML_Value;
            end if;

            From.Report_Extra_Keys;

            return New_Value (Type_Def);
         end;
      end Create_One;

      Raw : constant TOML_Value := From.Pop;
   begin
      if Raw.Kind /= TOML_Table then
         Raise_Checked_Error (TOML_Keys.Config_Vars & "must be a table");
      end if;

      return Props : Conditional.Properties do
         for Item of Iterate_On_Table (Raw) loop
            Props := Props and Create_One (To_String (Item.Key),
                                           Item.Value);
            Raw.Unset (Item.Key);
         end loop;
      end return;
   end Definitions_From_TOML;

   ----------------------------
   -- Assignements_From_TOML --
   ----------------------------

   function Assignments_From_TOML (From : TOML_Adapters.Key_Queue)
                                    return Conditional.Properties
   is
      use Conditional.For_Properties;

      ----------------
      -- Create_One --
      ----------------

      function Create_One (Crate : String;
                           Raw   : TOML.TOML_Value)
                           return Conditional.Properties
      is
         Val : Config_Value_Assignment;
      begin
         if Raw.Kind /= TOML_Table then
            Raise_Checked_Error
              (TOML_Keys.Config_Sets & " assignements must be a table");
         end if;

         Val.Crate := +Crate;

         for Item of Iterate_On_Table (Raw) loop
            declare
               Assign : Assignment;
            begin
               Assign.Name := Item.Key;
               Assign.Value := Item.Value;

               Val.List.Append (Assign);
            end;
         end loop;
         return New_Value (Val);
      end Create_One;

      Raw : constant TOML_Value := From.Pop;
   begin
      if Raw.Kind /= TOML_Table then
         raise Checked_Error with TOML_Keys.Config_Sets & " must be a table";
      end if;

      return Props : Conditional.Properties do
         for Item of Iterate_On_Table (Raw) loop
            Props := Props and Create_One (To_String (Item.Key), Item.Value);
            Raw.Unset (Item.Key);
         end loop;
      end return;
   end Assignments_From_TOML;

   ----------------------------
   -- Config_Entry_From_TOML --
   ----------------------------

   function Config_Entry_From_TOML (From : TOML_Adapters.Key_Queue)
                                    return Conditional.Properties
   is
      Config : constant TOML_Adapters.Key_Queue :=
                 From.Descend
                   (From.Checked_Pop
                      (TOML_Keys.Configuration, TOML_Table),
                    TOML_Keys.Configuration);

   begin
      return Props : Conditional.Properties do
         while True loop
            declare
               Val    : TOML_Value;
               Key    : constant String := Config.Pop (Val);
               Nested : Conditional.Properties;
               --  For nested tables under [configuration]
            begin
               exit when Key = "";

               if Key = TOML_Keys.Config_Vars then
                  Nested := Definitions_From_TOML
                    (From.Descend (Key, Val, TOML_Keys.Config_Vars));
               elsif Key = TOML_Keys.Config_Sets then
                  Nested := Assignments_From_TOML
                    (From.Descend (Key, Val, TOML_Keys.Config_Vars));
               else
                  Raise_Checked_Error ("Unknown configuration entry: "
                                       & Key);
               end if;

               Props.Append (Nested);
            end;
         end loop;

         if Props.Is_Empty then
            Props := Conditional.New_Property
              (Config_Entry'(Property with null record));
         end if;
      end return;
   end Config_Entry_From_TOML;

end Alire.Properties.Configurations;
