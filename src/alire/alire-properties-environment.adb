with Alire.TOML_Keys;

package body Alire.Properties.Environment is

   ------------
   -- Action --
   ------------

   function Action (This : Variable) return Actions is
     (This.Action);

   ----------
   -- Name --
   ----------

   function Name (This : Variable) return String is
     (+This.Name);

   function Shell_Name (This : Variable) return String is
     ("${" & Name (This) & "}");

   -----------
   -- Value --
   -----------

   function Value (This : Variable) return String is
     (+This.Value);

   ---------------
   -- Image_RHS --
   ---------------

   function Image_RHS (This : Variable) return String is
     (Name (This)
      & (case This.Action is
            when Append  => "=" & Shell_Name (This)  & ":"  & Value (This),
            when Prepend => "=" & Value (This) & ":" & Shell_Name (This),
            when Set     => "=" & Value (This)));

   -----------
   -- Image --
   -----------

   overriding
   function Image (This : Variable) return String is
      ("Environment: " & Image_RHS (This));

   ---------
   -- Key --
   ---------

   overriding
   function Key (This : Variable) return String is
      pragma Unreferenced (This);
   begin
      return TOML_Keys.Environment;
   end Key;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties
   is
      use type Conditional.Properties;
      use TOML;
      Env : TOML_Value;
   begin
      if From.Unwrap.Kind /= TOML_Table then
         From.Checked_Error
           ("environment: table with assignments expected, but got: "
            & From.Unwrap.Kind'Img);
      end if;

      if From.Pop_Single_Table (Env, TOML_Table) /= TOML_Keys.Environment then
         raise Program_Error;
         --  Can't happen, unless the dispatch to us itself was erroneous
      end if;

      return Props : Conditional.Properties do
         for Name of Env.Keys loop
            declare
               Var  : Variable;   -- The env. var. being parsed
               Val  : TOML_Value; -- The env. var. value
            begin
               Var.Name := Name;

               --  Action

               declare
                  Action_Image : constant String :=
                                   From.Descend
                                     (Value   => Env.Get (Name),
                                      Context => "environment: " & (+Name))
                                     .Pop_Single_Table (Val, TOML_String);
               begin
                  Var.Action := Actions'Value (Action_Image);
               exception
                  when Constraint_Error => -- because Action is invalid
                     From.Checked_Error
                       (": " & (+Var.Name)
                        & "invalid action: " & Action_Image);
               end;

               --  Value (already type checked in previous pop)

               Var.Value := +Val.As_String;

               --  Pop entry to avoid upper "unexpected key" errors

               Env.Unset (+Name);

               --  Final assignment

               Props := Props and Var;
            end;
         end loop;

      end return;
   end From_TOML;

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (This : Variable) return TOML.TOML_Value is
      use TOML;
      Child : constant TOML_Value := Create_Table;
   begin
      return Result : constant TOML_Value := Create_Table do

         --  Create the VAR.action.Value nested tables

         Child.Set (AAA.Strings.To_Lower_Case (This.Action'Img),
                    Create_String (Value (This)));

         Result.Set (Name (This),
                     Child);
      end return;
   end To_TOML;

   -------------
   -- To_YAML --
   -------------

   overriding
   function To_YAML (This : Variable) return String is
     ("Environment: '" & Image_RHS (This) & "'");

end Alire.Properties.Environment;
