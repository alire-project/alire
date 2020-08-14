package body Alire.TOML_Adapters is

   ------------
   -- Assert --
   ------------

   procedure Assert (Queue : Key_Queue; Condition : Boolean; Message : String)
   is
   begin
      if not Condition then
         Queue.Checked_Error (Message);
      end if;
   end Assert;

   ----------------
   -- Assert_Key --
   ----------------

   procedure Assert_Key (Queue : Key_Queue;
                         Key   : String;
                         Kind  : TOML.Any_Value_Kind) is
   begin
      if not Queue.Value.Has (Key) then
         Queue.Checked_Error ("missing required field: " & Key);
      elsif Queue.Value.Get (Key).Kind not in Kind then
         Queue.Checked_Error ("field " & Key & ": expected a "
                              & Kind'Img
                              & " but got a "
                              & Queue.Value.Get (Key).Kind'Img);
      end if;
   end Assert_Key;

   -------------------
   -- Checked_Error --
   -------------------

   procedure Checked_Error (Queue : Key_Queue; Message : String) is
   begin
      raise Alire.Checked_Error with Errors.Set (Queue.Message (Message));
   end Checked_Error;

   -----------------------
   -- Recoverable_Error --
   -----------------------

   procedure Recoverable_Error (Queue   : Key_Queue;
                                Message : String;
                                Recover : Boolean := Alire.Force)
   is
   begin
      if Recover then
         Recoverable_Error (Queue.Message (Message), Recover);
      else
         Queue.Checked_Error (Message);
      end if;
   end Recoverable_Error;

   -----------------
   -- Checked_Pop --
   -----------------

   function Checked_Pop (Queue : Key_Queue;
                         Key   : String;
                         Kind  : TOML.Any_Value_Kind)
                         return TOML.TOML_Value is
   begin
      Queue.Assert_Key (Key, Kind);
      return Value : TOML.TOML_Value do
         if not Queue.Pop (Key, Value) then
            raise Program_Error with ("missing key, but it was just checked?");
         end if;
      end return;
   end Checked_Pop;

   ----------
   -- From --
   ----------

   function From (Value   : TOML.TOML_Value;
                  Context : String) return Key_Queue is
     (Value, +Context);

   ----------
   -- From --
   ----------

   function From (Key     : String;
                  Value   : TOML.TOML_Value;
                  Context : String) return Key_Queue
   is
      Table : constant TOML.TOML_Value := TOML.Create_Table;
   begin
      Table.Set (Key, Value);
      return From (Table, Context);
   end From;

   -------------
   -- Descend --
   -------------

   function Descend (Parent  : Key_Queue;
                     Value   : TOML.TOML_Value;
                     Context : String) return Key_Queue is
     (From (Value, (+Parent.Context) & ASCII.LF & Context));

   ---------
   -- Pop --
   ---------

   function Pop (Queue : Key_Queue) return TOML.TOML_Value is
      Val : TOML.TOML_Value;
      Key : constant String := Queue.Pop (Val);
   begin
      if Key /= "" then
         return Val;
      else
         return TOML.No_TOML_Value;
      end if;
   end Pop;

   ---------
   -- Pop --
   ---------

   function Pop (Queue :        Key_Queue;
                 Value :    out TOML.TOML_Value) return String is
   begin
      --  Use first of remaining keys
      for Key of Queue.Value.Keys loop
         Value := Queue.Value.Get (Key);
         Queue.Value.Unset (Key);
         return +Key;
      end loop;

      --  If no keys left...
      return "";
   end Pop;

   ---------
   -- Pop --
   ---------

   function Pop (Queue : Key_Queue;
                 Key   : String;
                 Value : out TOML.TOML_Value) return Boolean
   is
      use TOML;
   begin
      if Queue.Value /= No_TOML_Value and then Queue.Value.Kind = TOML_Table
      then
         Value := Queue.Value.Get_Or_Null (Key);
         if Value /= No_TOML_Value then
            Queue.Value.Unset (Key);
         end if;
         return Value /= TOML.No_TOML_Value;
      else
         return False;
      end if;
   end Pop;

   --------------
   -- Pop_Expr --
   --------------

   function Pop_Expr (Queue  : Key_Queue;
                      Prefix : String;
                      Value  : out TOML.TOML_Value) return String
   is
   begin
      Value := TOML.No_TOML_Value;

      if Queue.Value.Kind /= TOML.TOML_Table then
         return "";
      end if;

      for Pair of Queue.Value.Iterate_On_Table loop
         if Utils.Starts_With (+Pair.Key, Prefix) then
            return Key : constant String := +Pair.Key do
               Value := Pair.Value;
               Queue.Value.Unset (Pair.Key);
            end return;
         end if;
      end loop;

      return "";
   end Pop_Expr;

   ----------------------
   -- Pop_Single_Table --
   ----------------------

   function Pop_Single_Table (Queue : Key_Queue;
                              Value : out TOML.TOML_Value;
                              Kind  : TOML.Any_Value_Kind) return String
   is
      use TOML;
   begin
      if Queue.Value.Kind /= TOML_Table then
         Queue.Checked_Error ("expected a table but got a "
                              & Queue.Value.Kind'Img);
      end if;

      if Queue.Value.Keys'Length /= 1 then
         Queue.Checked_Error ("expected a single entry in table, but got"
                              & Queue.Value.Keys'Length'Img);
      end if;

      Value := Queue.Value.Get (Queue.Value.Keys (1));

      if Value.Kind /= Kind then
         Queue.Checked_Error ("expected a single entry of type "
                              & Kind'Img & ", but got a " & Value.Kind'Img);
      end if;

      return Key : constant String := +Queue.Value.Keys (1) do
         Queue.Value.Unset (Queue.Value.Keys (1));
      end return;
   end Pop_Single_Table;

   -----------------------
   -- Report_Extra_Keys --
   -----------------------

   function Report_Extra_Keys (Queue : Key_Queue) return Outcome
   is
      use UStrings;
      Message  : UString := +Errors.Wrap (+Queue.Context,
                                          "forbidden extra entries: ");
      Is_First : Boolean := True;
      Errored  : Boolean := False;
   begin
      for Key of Queue.Value.Keys loop
         Errored := True;
         if Is_First then
            Is_First := False;
         else
            UStrings.Append (Message, ", ");
         end if;
         UStrings.Append (Message, Key);
      end loop;

      if Errored then
         if Force then
            Recoverable_Error (+Message);
            return Outcome_Success;
         else
            return Outcome_Failure (+Message);
         end if;
      else
         return Outcome_Success;
      end if;
   end Report_Extra_Keys;

   -----------------------
   -- Report_Extra_Keys --
   -----------------------

   procedure Report_Extra_Keys (Queue : Key_Queue) is
      Result : constant Outcome := Queue.Report_Extra_Keys;
   begin
      if not Force and then not Result.Success then
         raise Alire.Checked_Error with Errors.Set (Message (Result));
      end if;
   end Report_Extra_Keys;

   -----------
   -- Print --
   -----------

   procedure Print (Queue : Key_Queue) is
      use TOML;

      function Image (Val : TOML_Value) return String is
        (case Val.Kind is
            when TOML_String => Val.As_String,
            when TOML_Boolean => Val.As_Boolean'Img,
            when TOML_Integer => Val.As_Integer'Img,
            when others       => raise Unimplemented);

      procedure Print (Val : TOML_Value; Prefix : String) is
      begin
         case Val.Kind is
            when TOML_Table =>
               Trace.Always (Prefix & "table:");
               for Pair of Val.Iterate_On_Table loop
                  if Pair.Value.Kind in Atom_Value_Kind then
                     Trace.Always (Prefix & (+Val.Keys (1)) & " = "
                                   & Image (Pair.Value));
                  else
                     Trace.Always (Prefix & (+Pair.Key) & " = ");
                     Print (Pair.Value, Prefix & "   ");
                  end if;
               end loop;
            when TOML_Array =>
               Trace.Always (Prefix & "array(1 .." & Val.Length'Img
                             & ") "
                             & (if Val.Length > 0
                               then " of " & Val.Item (1).Kind'Img
                               else ""));
            when others =>
               Trace.Always (Prefix & "value: " & Val.Kind'Img);
         end case;
      end Print;

   begin
      Print (Queue.Value, "");
   end Print;

   ---------
   -- "+" --
   ---------

   function "+" (Vect : Utils.String_Vector) return TOML.TOML_Value is
      Result : constant TOML.TOML_Value := TOML.Create_Array;
   begin
      for Str of Vect loop
         Result.Append (TOML.Create_String (Str));
      end loop;
      return Result;
   end "+";

   --------------
   -- To_Array --
   --------------

   function To_Array (V : TOML.TOML_Value) return TOML.TOML_Value is
      use TOML;
   begin
      if V.Kind = TOML_Array then
         return V;
      else
         declare
            Arr : constant TOML_Value := Create_Array (V.Kind);
         begin
            Arr.Append (V);
            return Arr;
         end;
      end if;
   end To_Array;

   --------------
   -- To_Table --
   --------------

   function To_Table (Key : String;
                      Val : TOML.TOML_Value) return TOML.TOML_Value is
      use TOML;
   begin
      return Table : constant TOML_Value := Create_Table do
         Table.Set (Key, Val);
      end return;
   end To_Table;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (Val : TOML.TOML_Value) return Utils.String_Vector is
      use Alire.Utils;

      Result : String_Vector := Empty_Vector;
   begin
      for I in 1 .. Val.Length loop
         Result.Append (Val.Item (I).As_String);
      end loop;
      return Result;
   end To_Vector;

end Alire.TOML_Adapters;
