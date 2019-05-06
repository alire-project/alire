with Alire.TOML_Adapters;

package body Alire.Properties.Scenarios is

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
