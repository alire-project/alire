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

   overriding
   function To_TOML (This : Run) return TOML.TOML_Value is

      use TOML_Adapters;

      function Tomify is new TOML_Adapters.Tomify_Enum (Moments);

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

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties is

      --  Actions come in a TOML array.

      use Conditional.For_Properties;
      use TOML;

      ----------------
      -- Create_One --
      ----------------

      function Create_One (Raw : TOML.TOML_Value)
                           return Conditional.Properties
      is
         From    : constant TOML_Adapters.Key_Queue :=
                     From_TOML.From.Descend (Raw, "action");
         Kind    : TOML_Value;
         Command : TOML_Value;
         Path    : TOML_Value;
         Used    : Boolean;
      begin
         if not From.Pop (TOML_Keys.Action_Type, Kind) then
            From.Checked_Error ("action type missing");
         elsif not From.Pop (TOML_Keys.Action_Command, Command) then
            From.Checked_Error ("action command missing");
         end if;

         Used := From.Pop (TOML_Keys.Action_Folder, Path);
         --  The path key for an action is optional.

         if Kind.Kind /= TOML_String
           or else (Used and then Path.Kind /= TOML_String)
         then
            From.Checked_Error ("actions type and folder must be strings");
         end if;

         if Command.Kind /= TOML_Array
           or else
            Command.Length = 0
           or else
            Command.Item (1).Kind /= TOML_String
         then
            From.Checked_Error
              ("actions command must be an array of string(s)");
         end if;

         From.Report_Extra_Keys;

         return New_Value
           (New_Run
              (Moment                =>
                 Moments'Value (TOML_Adapters.Adafy (Kind.As_String)),

               Relative_Command_Line =>
                 TOML_Adapters.To_Vector (TOML_Adapters.To_Array (Command)),

               Working_Folder        =>
                 (if Used then Path.As_String else ".")));
      end Create_One;

      Raw : constant TOML_Value := From.Pop;

   begin
      if Raw.Kind = TOML_Table then
         return Create_One (Raw);
      end if;

      --  It should be an array that we we'll load one by one:
      if Raw.Kind /= TOML_Array then
         raise Checked_Error with "actions must be a table array";
      end if;

      --  An empty array can be used for actions:
      if Raw.Length = 1 and then Raw.Item (1).Keys'Length = 0 then
         Trace.Debug ("Skipping empty action array");
         return Conditional.No_Properties;
      end if;

      return Props : Conditional.Properties do
         for I in 1 .. Raw.Length loop
            Props := Props and Create_One (Raw.Item (I));
         end loop;
      end return;
   end From_TOML;

end Alire.Actions;
