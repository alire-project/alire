package body Alire.TOML_Adapters is

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (This : in out Key_Queue) is
      pragma Unreferenced (This);
   begin
      --  Manually close this error scope
      Errors.Close;
   end Finalize;

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
      pragma Unreferenced (Queue);
   begin
      raise Alire.Checked_Error with Errors.Set (Message);
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
         Recoverable_Error (Message, Recover);
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

   ------------------
   -- Create_Table --
   ------------------

   function Create_Table (Key   : String;
                          Value : TOML.TOML_Value)
                          return TOML.TOML_Value
   is
   begin
      return Table : constant TOML.TOML_Value := TOML.Create_Table do
         Table.Set (Key, Value);
      end return;
   end Create_Table;

   ----------
   -- From --
   ----------

   function From (Value   : TOML.TOML_Value;
                  Context : String) return Key_Queue
   is
   begin
      return This : constant Key_Queue :=
        (Ada.Finalization.Limited_Controlled with Value => Value)
      do
         Errors.Open (Context);
      end return;
   end From;

   ----------
   -- From --
   ----------

   function From (Key     : String;
                  Value   : TOML.TOML_Value;
                  Context : String) return Key_Queue
   is (From (Create_Table (Key, Value),
             Context));

   -------------
   -- Descend --
   -------------

   function Descend (Parent  : Key_Queue;
                     Value   : TOML.TOML_Value;
                     Context : String) return Key_Queue is
     (From (Value, Context));

   ------------------
   -- Merge_Tables --
   ------------------

   function Merge_Tables (L, R : TOML.TOML_Value) return TOML.TOML_Value
   is
      use TOML;
      function Merge_Internal (Key  : Unbounded_UTF8_String;
                      L, R : TOML_Value)
                      return TOML_Value
      is
         pragma Unreferenced (Key);
      begin
         if L.Kind = TOML_Table and then R.Kind = L.Kind then
            return TOML.Merge (L, R, Merge_Internal'Access);
         else
            Raise_Checked_Error
              ("Ill-shaped TOML information cannot be merged");
         end if;
      end Merge_Internal;
   begin
      return TOML.Merge (L, R, Merge_Entries => Merge_Internal'Access);
   end Merge_Tables;

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

   ---------
   -- Pop --
   ---------

   function Pop (Queue : Key_Queue; Key : String) return TOML.TOML_Value is
      Val : TOML.TOML_Value;
   begin
      if not Queue.Pop (Key, Val) then
         Raise_Checked_Error ("Requested key not found: " & Key);
      end if;
      return Val;
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
         if AAA.Strings.Has_Prefix (+Pair.Key, Prefix) then
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
                              Value : out TOML.TOML_Value)
                              return String
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

      return Key : constant String := +Queue.Value.Keys (1) do
         Queue.Value.Unset (Queue.Value.Keys (1));
      end return;
   end Pop_Single_Table;

   ----------------------
   -- Pop_Single_Table --
   ----------------------

   function Pop_Single_Table (Queue : Key_Queue;
                              Value : out TOML.TOML_Value;
                              Kind  : TOML.Any_Value_Kind) return String
   is
      use TOML;
      Key : constant String := Queue.Pop_Single_Table (Value);
   begin
      if Value.Kind /= Kind then
         Queue.Checked_Error ("expected a single entry of type "
                              & Kind'Img & ", but got a " & Value.Kind'Img);
      end if;

      return Key;
   end Pop_Single_Table;

   -----------------------
   -- Report_Extra_Keys --
   -----------------------

   function Report_Extra_Keys (Queue : Key_Queue) return Outcome
   is
      use UStrings;
      Message  : UString := +"forbidden extra entries: ";
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
            when TOML_String  => Val.As_String,
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
                     Trace.Always (Prefix & (+Pair.Key) & " = "
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
               for I in 1 .. Val.Length loop
                  Trace.Always (Prefix & "array entry" & I'Img & ":");
                  Print (Val.Item (I), Prefix & "   ");
               end loop;
            when others =>
               Trace.Always (Prefix & "value: " & Val.Kind'Img
                             & "; img: "
                             & (case Val.Kind is
                                  when TOML_String  => Val.As_String,
                                  when TOML_Integer => Val.As_Integer'Image,
                                  when TOML_Boolean => Val.As_Boolean'Image,
                                  when others       => "(unimplemented)"));
         end case;
      end Print;

   begin
      Trace.Always ("Printing context: " & Errors.Stack (""));
      Print (Queue.Value, "");
   end Print;

   ---------
   -- "+" --
   ---------

   function "+" (Vect : AAA.Strings.Vector) return TOML.TOML_Value is
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
            Arr : constant TOML_Value := Create_Array;
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

   function To_Vector (Val : TOML.TOML_Value) return AAA.Strings.Vector is

      Result : Vector := Empty_Vector;
   begin
      for I in 1 .. Val.Length loop
         Result.Append (Val.Item (I).As_String);
      end loop;
      return Result;
   end To_Vector;

end Alire.TOML_Adapters;
