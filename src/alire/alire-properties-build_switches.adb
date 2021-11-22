with Alire.TOML_Keys;
with Alire.Utils.Switches; use Alire.Utils.Switches;

package body Alire.Properties.Build_Switches is

   -----------
   -- Image --
   -----------

   overriding
   function Image (This : Variable) return String is
      ("Build Switches: ");

   ---------
   -- Key --
   ---------

   overriding
   function Key (This : Variable) return String is
      pragma Unreferenced (This);
   begin
      return TOML_Keys.Build_Profile;
   end Key;

   --------------------
   -- Parse_Switches --
   --------------------

   function Parse_Switches (From : TOML_Adapters.Key_Queue;
                            Cat  : Switches_Categories;
                            V    : TOML.TOML_Value)
                            return Switch_List
   is
      pragma Unreferenced (Cat);
      Result : Switch_List;
   begin
      case V.Kind is
         when TOML_String =>
            return Empty_List;
         when TOML_Array =>
            for Index in 1 .. V.Length loop
               Result.Append (V.Item (Index).As_String);
            end loop;
         when others =>
            From.Checked_Error
              ("Invalid kind for build_switches category. " &
                 "String or Array of String expected");
      end case;
      return Result;
   end Parse_Switches;

   pragma Unreferenced (Parse_Switches);

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties
   is
      use type Conditional.Properties;
      --  use TOML;
      --  Env : TOML_Value;
      --
      --  Profiles_Names : constant AAA.Strings.Vector :=
      --    AAA.Strings.Empty_Vector
      --      .Append ("*")
      --      .Append ("crate")
      --      .Append ("release")
      --      .Append ("validation")
      --      .Append ("dev");
      --
      --  Categories_Names : constant AAA.Strings.Vector :=
      --    AAA.Strings.Empty_Vector
      --      .Append ("optimize")
      --      .Append ("debug_info")
      --      .Append ("runtime_checks")
      --      .Append ("style_checks")
      --      .Append ("contracts");

      Var : Variable;
   begin
      if From.Unwrap.Kind /= TOML_Table then
         From.Checked_Error
           ("Build_switches: table with assignments expected, but got: "
            & From.Unwrap.Kind'Img);
      end if;

      return Props : Conditional.Properties do
         Var.T := From.Unwrap;
         Props := Props and Var;
      end return;

   --  if From.Pop_Single_Table (Env, TOML_Table) /= TOML_Keys.Build_Switches
      --  then
      --     raise Program_Error;
      --     --  Can't happen, unless the dispatch to us itself was erroneous
      --  end if;
      --
      --  return Props : Conditional.Properties do
      --     for Profile of Env.Keys loop
      --        declare
      --           Var  : Variable;   -- The env. var. being parsed
      --           Categories : constant TOML_Value := Env.Get (Profile);
      --
      --        begin
      --
   --           Trace.Always ("Build switches: Profile: '" & (+Profile) & "'");
      --
      --           if not Profiles_Names.Contains (+Profile) then
      --              From.Checked_Error
      --                ("Invalid build profile name: '" & (+Profile) & "'");
      --           end if;
      --
      --           if Categories.Kind /= TOML_Table then
      --              From.Checked_Error ("Should be table");
      --           end if;
      --
      --           for Category of Categories.Keys loop
      --              Trace.Always ("Category: '" & (+Category) & "'");
      --
      --              if not Categories_Names.Contains (+Category) then
      --                 From.Checked_Error
   --                   ("Invalid build category name: '" & (+Category) & "'");
      --              end if;
      --
      --              declare
      --                 Cat : constant Switches_Categories
      --                   := Switches_Categories'Value (+Category);
      --
   --                 Value : constant TOML_Value := Categories.Get (Category);
      --
      --                 Switches : constant Switch_List :=
      --                   Parse_Switches (From, Cat, Value);
      --              begin
      --
      --                 if (+Profile) = "*" then
      --                    for P in Build_Profiles loop
      --                       Var.Switches (P) (Cat) := Switches;
      --                       Trace.Always
      --                         (P'Img & "." & Cat'Img & " := [" &
   --                            Var.Switches (P) (Cat).Flatten (", ") & "]");
      --                    end loop;
      --                 else
      --                    declare
      --                       P : constant Build_Profiles :=
      --                         Build_Profiles'Value (+Profile);
      --                    begin
      --                       Var.Switches (P) (Cat) := Switches;
      --                       Trace.Always
      --                         (P'Img & "." & Cat'Img & " := [" &
   --                            Var.Switches (P) (Cat).Flatten (", ") & "]");
      --                    end;
      --                 end if;
      --              end;
      --
      --           end loop;
      --
      --           --  Pop entry to avoid upper "unexpected key" errors
      --
      --           Env.Unset (+Profile);
      --
      --           --  Final assignment
      --
      --           Props := Props and Var;
      --        end;
      --     end loop;
      --
      --  end return;
   end From_TOML;

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (This : Variable) return TOML.TOML_Value is
      --  use TOML;
      --  --  Child : constant TOML_Value := Create_Table;
   begin
      return This.T;
      --  return Result : constant TOML_Value := Create_Table do
      --
      --     --  Create the VAR.action.Value nested tables
      --
      --     --  Child.Set (AAA.Strings.To_Lower_Case (This.Action'Img),
      --     --             Create_String (Value (This)));
      --
      --     --  Result.Set (This.Name, Child);
      --     null;
      --  end return;
   end To_TOML;

   -------------
   -- To_YAML --
   -------------

   overriding
   function To_YAML (This : Variable) return String is
     ("Build switches: 'TODO'");

end Alire.Properties.Build_Switches;
