
with TOML; use TOML;

with Alire.Utils.YAML;

with Ada.Characters.Handling;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Alire.Properties.Templates is

   use AAA.Strings;

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
         return Templates.Str;
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
      for Index in 1 .. Str_Array.Length loop
         if Str_Array.Item (Index).Kind /= TOML_String then
            raise Program_Error with "Invalid TOML kind for enum values";
         end if;

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
            return To_Lower_Case (Val.As_Boolean'Img);
         when TOML_Float =>
            return Image (Val.As_Float);
         when TOML_Integer =>
            return Image (Val.As_Integer);

         when TOML_Array =>
            --  Type of elements is checked inside following call
            return To_String (Val);
         when others =>
            Raise_Checked_Error ("Unexpected kind '" & Val.Kind'Img &
                                   "' in conversion to String");
      end case;
   end Image;

   --------------------
   -- Type_To_String --
   --------------------

   function Type_To_String (This : Input_Definition) return String
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
   function Image (This : Input_Definition) return String is
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
   function To_TOML (This : Input_Definition) return TOML.TOML_Value is
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
   function To_YAML (This : Input_Definition) return String is
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
   -- Valid --
   -----------

   function Valid (This : Input_Definition;
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
               return
                 (for some Index in 1 .. This.Values.Length =>
                    This.Values.Item (Index).As_String = Str);
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

   function Name (This : Input_Definition) return String
   is (+This.Name);

   -------------------------
   -- Templates_From_TOML --
   -------------------------

   function Templates_From_TOML (From : TOML_Adapters.Key_Queue)
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
           TOML_Adapters.From (Raw, TOML_Keys.Template_Inputs & "." & Name);
         Typ     : TOML_Value;
         First   : TOML_Value;
         Last    : TOML_Value;

         Unused : Boolean;

      begin
         Trace.Always ("Create_One -> " & Name);
         if Raw.Kind /= TOML_Table then
            From.Checked_Error ("inputs definition must be a table: " &
                                  Raw.Kind'Img);
         end if;

         if not From.Pop ("type", Typ) then
            From.Checked_Error ("'type' missing");
         end if;

         if Typ.Kind /= TOML_String then
            From.Checked_Error ("'type' must be string");
         end if;

         declare
            Type_Def : Input_Definition (To_Type (Typ.As_String));
         begin
            Type_Def.Name := +Name;

            case Type_Def.Kind is

               when Int =>
                  if From.Pop ("first", First) then
                     if First.Kind /= TOML_Integer then
                        From.Checked_Error ("first must be integer");
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
                       or else Type_Def.Values.Length = 0
                       or else (for some I in 1 .. Type_Def.Values.Length =>
                                  Type_Def.Values.Item (I).Kind /= TOML_String)
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
               From.Checked_Error ("'default' missing");
            end if;

            From.Report_Extra_Keys;

            return New_Value (Type_Def);
         end;
      end Create_One;

      Raw : constant TOML_Value := From.Pop;
   begin
      if Raw.Kind /= TOML_Table then
         Raise_Checked_Error (TOML_Keys.Template_Inputs & "must be a table");
      end if;

      return Props : Conditional.Properties do
         for Item of Iterate_On_Table (Raw) loop
            Trace.Error (To_String (Item.Key));

            Props := Props and Create_One (To_String (Item.Key),
                                           Item.Value);
            Raw.Unset (Item.Key);
         end loop;
      end return;
   end Templates_From_TOML;

   -----------------------
   -- Builtin_From_Enum --
   -----------------------

   function Typedef_From_Enum return Input_Definition is

      Ret : constant Input_Definition :=
        (Kind    => Enum,
         Name    => +Type_Name,
         Default => No_TOML_Value,
         Values  => TOML.Create_Array);
   begin
      for P in T loop
         if Lower_Case then
            Ret.Values.Append (TOML.Create_String (To_Lower_Case (P'Img)));
         else
            Ret.Values.Append (TOML.Create_String (P'Img));
         end if;
      end loop;
      return Ret;
   end Typedef_From_Enum;

   --------------------
   -- String_Typedef --
   --------------------

   function String_Typedef (Name : String) return Input_Definition
   is (Kind    => Str,
       Name    => +Name,
       Default => No_TOML_Value);

end Alire.Properties.Templates;
