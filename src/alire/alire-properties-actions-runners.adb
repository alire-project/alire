with AAA.Enum_Tools;

package body Alire.Properties.Actions.Runners is

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
      if This.Name /= "" then
         Table.Set (TOML_Keys.Name, +This.Name);
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

      function Create_One (Raw : TOML.TOML_Value; Index : Positive)
                           return Conditional.Properties
      is
         From    : constant TOML_Adapters.Key_Queue :=
                     From_TOML.From.Descend (Raw, "action #"
                                             & Utils.Trim (Index'Image));
         Kind     : TOML_Value;
         Command  : TOML_Value;
         Name     : TOML_Value;
         Has_Name : Boolean;
         Path     : TOML_Value;
         Has_Path : Boolean;
         Moment   : Moments;

         function Is_Valid is new AAA.Enum_Tools.Is_Valid (Moments);
      begin
         if not From.Pop (TOML_Keys.Action_Type, Kind) then
            From.Checked_Error ("action type missing");
         elsif not From.Pop (TOML_Keys.Action_Command, Command) then
            From.Checked_Error ("action command missing");
         end if;

         Has_Path := From.Pop (TOML_Keys.Action_Folder, Path);
         --  The path key for an action is optional

         Has_Name := From.Pop (TOML_Keys.Name, Name);
         --  Name is optional but for custom actions

         if Kind.Kind /= TOML_String
           or else (Has_Path and then Path.Kind /= TOML_String)
         then
            From.Checked_Error ("actions type, and folder must be strings");
         end if;

         if not Is_Valid (TOML_Adapters.Adafy (Kind.As_String)) then
            From.Checked_Error ("action type is invalid: " & Kind.As_String);
         else
            Moment := Moments'Value (TOML_Adapters.Adafy (Kind.As_String));
         end if;

         if Moment = On_Demand and then not Has_Name then
            From.Checked_Error ("on-demand actions require a name");
         end if;

         if Has_Name and then
           (Name.Kind /= TOML_String or else Name.As_String not in Action_Name)
         then
            From.Checked_Error
              ("action name must be a string made of "
               & "'a' .. 'z', '0' .. '9', '-', starting with a letter and not "
               & "ending with a dash nor containing consecutive dashes"
               & ASCII.LF & "Offending name is: " & Name.As_String);
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
                 Moment,

               Name                  =>
                 (if Has_Name then Name.As_String else ""),

               Relative_Command_Line =>
                 TOML_Adapters.To_Vector (TOML_Adapters.To_Array (Command)),

               Working_Folder        =>
                 (if Has_Path then Path.As_String else ".")));
      end Create_One;

      Raw : constant TOML_Value := From.Pop;

   begin
      if Raw.Kind = TOML_Table then
         return Create_One (Raw, 1);
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
            Props := Props and Create_One (Raw.Item (I), I);
         end loop;
      end return;
   end From_TOML;

end Alire.Properties.Actions.Runners;
