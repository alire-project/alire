with AAA.Enum_Tools;

with TOML; use TOML;

with Alire.Utils.YAML;
with Alire.Utils.Did_You_Mean;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Alire.Properties.Configurations is

   use AAA.Strings;

   type Valid_Keys is (Output_Dir,
                       Disabled,
                       Generate_Ada,
                       Generate_GPR,
                       Generate_C,
                       Auto_GPR_With);

   function Is_Valid is new AAA.Enum_Tools.Is_Valid (Valid_Keys);
   function Valid_Keys_Suggestion
   is new Utils.Did_You_Mean.Enum_Suggestion
     (Valid_Keys, Utils.Did_You_Mean.Lower_Case);

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (This : Config_Entry) return TOML.TOML_Value is
      Table : constant TOML.TOML_Value := TOML.Create_Table;
   begin
      Table.Set ("output_dir", Create_String (This.Output_Dir));
      Table.Set ("disabled", Create_Boolean (This.Disabled));
      Table.Set ("generate_ada", Create_Boolean (This.Gen_Ada));
      Table.Set ("generate_gpr", Create_Boolean (This.Gen_GPR));
      Table.Set ("generate_c", Create_Boolean (This.Gen_C));
      Table.Set ("auto_gpr_with", Create_Boolean (This.Auto_GPR_With));
      return Table;
   end To_TOML;

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

         Append (Ret, +Elt.Name & " := " &
                   Utils.Config_Type_Def.Image (Elt.Value));
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
                   Utils.YAML.YAML_Stringify
                     (Utils.Config_Type_Def.Image (Elt.Value)) &
                "}");
      end loop;

      Append (Ret, "]}");
      return +Ret;
   end To_YAML;

   -----------
   -- Valid --
   -----------

   function Valid (This : Config_Variable;
                   Val  : TOML.TOML_Value)
                   return Boolean
   is (Utils.Config_Type_Def.Valid (This.Def, Val));

   ----------
   -- Name --
   ----------

   function Name (This : Config_Variable) return String
   is (+This.Def.Name);

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
         Res : Config_Variable;
      begin
         Res.Def := Utils.Config_Type_Def.Type_Def_From_TOML
           (Name, Raw, TOML_Keys.Config_Vars);

         return New_Value (Res);
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

   ---------------------------
   -- Assignments_From_TOML --
   ---------------------------

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
              (TOML_Keys.Config_Values & " assignments must be a table");
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
         raise Checked_Error with TOML_Keys.Config_Values & " must be a table";
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

      Ent : Config_Entry;
   begin
      return Props : Conditional.Properties do
         loop
            declare
               Val    : TOML_Value;
               Key    : constant String := Config.Pop (Val);
               Nested : Conditional.Properties;
               --  For nested tables under [configuration]
            begin
               exit when Key = "";

               if Key = Tail (TOML_Keys.Config_Vars, '.') then
                  Nested := Definitions_From_TOML
                    (From.Descend (Key, Val, "variables"));

               elsif Key = Tail (TOML_Keys.Config_Values, '.') then
                  Nested := Assignments_From_TOML
                    (From.Descend (Key, Val, "settings"));

               else
                  if Is_Valid (Key) then
                     case Valid_Keys'Value (Key) is
                        when Output_Dir =>
                           if Val.Kind = TOML_String then
                              Ent.Output_Dir := Val.As_Unbounded_String;
                           else
                              Raise_Checked_Error
                                ("invalid value for: " & Key &
                                   "(string expected)");
                           end if;

                        when Generate_Ada =>
                           if Val.Kind = TOML_Boolean then
                              Ent.Gen_Ada := Val.As_Boolean;
                           else
                              Raise_Checked_Error
                                ("invalid value for: " & Key &
                                   "(boolean expected)");
                           end if;

                        when Generate_GPR =>
                           if Val.Kind = TOML_Boolean then
                              Ent.Gen_GPR := Val.As_Boolean;
                           else
                              Raise_Checked_Error
                                ("invalid value for: " & Key &
                                                     "(Boolean expected)");
                           end if;

                        when Generate_C =>
                           if Val.Kind = TOML_Boolean then
                              Ent.Gen_C := Val.As_Boolean;
                           else
                              Raise_Checked_Error
                                ("invalid value for: " & Key &
                                   "(Boolean expected)");
                           end if;

                        when Auto_GPR_With =>
                           if Val.Kind = TOML_Boolean then
                              Ent.Auto_GPR_With := Val.As_Boolean;
                           else
                              Raise_Checked_Error
                                ("invalid value for: " & Key &
                                   "(Boolean expected)");
                           end if;

                        when Disabled =>
                           if Val.Kind = TOML_Boolean then
                              Ent.Disabled := Val.As_Boolean;
                           else
                              Raise_Checked_Error
                                ("invalid value for: " & Key &
                                   "(Boolean expected)");
                           end if;
                     end case;
                  else
                     Raise_Checked_Error ("Unknown configuration entry: '"
                                          & Key & "'." &
                                            Valid_Keys_Suggestion (Key));
                  end if;
               end if;
               Props.Append (Nested);
            end;
         end loop;

         Props.Append (Conditional.For_Properties.New_Value (Ent));
      end return;
   end Config_Entry_From_TOML;

end Alire.Properties.Configurations;
