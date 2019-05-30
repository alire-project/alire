with Alire.TOML_Adapters;

package body Alire.Actions is

   -------------
   -- Execute --
   -------------

   procedure Execute (This : Action;
                      Implementer : access procedure (This : Action'Class))
   is
   begin
      Implementer (This);
   end Execute;

   -------------
   -- To_TOML --
   -------------

   overriding function To_TOML (This : Run) return TOML.TOML_Value is

      use TOML_Adapters;

      function Tomify is new TOML_Adapters.Tomify (Moments);

      Arr : constant TOML.TOML_Value := TOML.Create_Array;
      --  Actions are output as an array of tables, so we return an array
      --    containing the single table of this action.
      Table : constant TOML.TOML_Value := TOML.Create_Table;
   begin
      Table.Set (TOML_Keys.Action_Type,    Tomify (This.Moment));
      Table.Set (TOML_Keys.Action_Command, +This.Command_Line);
      if This.Working_Folder /= "" then
         Table.Set (TOML_Keys.Action_Folder,  +This.Working_Folder);
      end if;
      Arr.Append (Table);
      return Arr;
   end To_TOML;

end Alire.Actions;
