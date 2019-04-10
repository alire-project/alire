with Alire.Utils;

package body Alire.TOML_Expressions is

   Case_Prefix : constant String := "case(";
   Case_Suffix : constant String := ")";

   function Process_Case_Clause
     (Values   : String;
      Callback : access function (Value : String) return Boolean)
      return Boolean;
   --  Assuming Values contains several fields separated by pipes, call
   --  Callback on each field. Stop as soon as Callback returns False. Return
   --  whether all Callback calls returned True.

   ----------------------------
   -- Is_Valid_Variable_Name --
   ----------------------------

   function Is_Valid_Variable_Name (Name : US.Unbounded_String) return Boolean
   is
      subtype Name_Index is Positive range 1 .. US.Length (Name);
      Last_Was_Underscore : Boolean := False;
   begin
      for I in Name_Index loop
         if I in Name_Index'First | Name_Index'Last or else Last_Was_Underscore
         then
            if US.Element (Name, I) not in 'a' .. 'z' then
               return False;
            end if;
            Last_Was_Underscore := False;

         elsif US.Element (Name, I) = '_' then
            Last_Was_Underscore := True;

         elsif US.Element (Name, I) not in 'a' .. 'z' then
            return False;

         else
            Last_Was_Underscore := True;
         end if;
      end loop;

      return True;
   end Is_Valid_Variable_Name;

   ----------------------------
   -- Is_Valid_Variable_Name --
   ----------------------------

   function Is_Valid_Variable_Name (Name : String) return Boolean is
   begin
      return Is_Valid_Variable_Name (+Name);
   end Is_Valid_Variable_Name;

   ----------------------
   -- Variable_Defined --
   ----------------------

   function Variable_Defined
     (Self : Environment_Variables;
      Name : US.Unbounded_String) return Boolean is
   begin
      return Self.Values.Contains (Name);
   end Variable_Defined;

   ------------------
   -- Add_Variable --
   ------------------

   procedure Add_Variable
     (Self      : in out Environment_Variables;
      Name      : US.Unbounded_String;
      Value_Set : String_Array;
      Value     : US.Unbounded_String)
   is
      Set : String_Set_Access := new String_Sets.Set;
   begin
      --  Build Set from values in Value_Set

      for V of Value_Set loop
         if Set.Contains (V) then
            Free (Set);
            raise Constraint_Error with "duplicate variable value";
         end if;
         Set.Insert (V);
      end loop;

      --  Check that Value is an allowed value

      if not Set.Contains (Value) then
         Free (Set);
         raise Constraint_Error with "value not in set";
      end if;

      --  Finally add the entry

      Self.Value_Sets.Insert (Name, Set);
      Self.Values.Insert (Name, Value);
   end Add_Variable;

   --------------------
   -- Variable_Value --
   --------------------

   function Variable_Value
     (Self : Environment_Variables;
      Name : US.Unbounded_String) return US.Unbounded_String is
   begin
      return Self.Values.Element (Name);
   end Variable_Value;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Environment_Variables) is
   begin
      --  Free dynamically allocated String_Set.Set objects in Self.Value_Sets

      for VS in Self.Value_Sets.Iterate loop
         declare
            Set : String_Set_Access := Variable_Value_Sets.Element (VS);
         begin
            Free (Set);
         end;
      end loop;
   end Finalize;

   -------------------------
   -- Process_Case_Clause --
   -------------------------

   function Process_Case_Clause
     (Values   : String;
      Callback : access function (Value : String) return Boolean)
      return Boolean
   is
      First : Positive := Values'First;
   begin
      for I in Values'Range loop
         if Values (I) = '|' then
            if not Callback (Values (First .. I - 1)) then
               return False;
            end if;
            First := I + 1;
         end if;
      end loop;
      return Callback (Values (First .. Values'Last));
   end Process_Case_Clause;

   package body Single_Values is

      -----------
      -- Parse --
      -----------

      function Parse
        (Value   : TOML.TOML_Value;
         Env     : Environment_Variables;
         Default : Optional_Value := (Present => False)) return Parsing_Result
      is

         Success : Boolean := True;
         Error   : US.Unbounded_String;

         Nodes : Node_Vectors.Vector;
         --  List of nodes we allocate in this function. This gives a simple
         --  method to free memory in case of error.

         function Is_Case_TOML
           (Value    : TOML.TOML_Value;
            Variable : out US.Unbounded_String;
            Mappings : out TOML.TOML_Value) return Boolean;
         --  Determine if TOML is a valid "case" expression pattern:
         --
         --     {"case(VAR)": {...}}
         --
         --  If it is not, just return false.
         --
         --  If it is, return true and set Variable to the name represented as
         --  VAR above, and set Mappings to the TOML object represented as
         --  {...} above.
         --
         --  If we found a "case(VAR)" field but it's not the only field or if
         --  it is not associated with a TOML object, put an error in Result
         --  and return false.

         function Parse_Node (Value : TOML.TOML_Value) return Expression_Node;
         --  TODO

         procedure Parse_Clause
           (Values   : String;
            Expr     : TOML.TOML_Value;
            Matchers : in out Matcher_Maps.Map;
            Default  : in out Expression_Node);
         --  Parser to be called on all associations inside a case expression.
         --  Name contains the various values to match (A|B for A or B) and
         --  Value is the expression associated. The corresponding entries are
         --  inserted into Matchers.
         --
         --  This assigns an error to Success/Error if anything invalid
         --  happens, and does nothing in the first place if Success is false.

         ------------------
         -- Is_Case_TOML --
         ------------------

         function Is_Case_TOML
           (Value    : TOML.TOML_Value;
            Variable : out US.Unbounded_String;
            Mappings : out TOML.TOML_Value) return Boolean
         is
            type Triboolean is (Unknown, False, True);
            Is_Case_TOML : Triboolean := Unknown;

            procedure Set_Error;
            --  Assign an error to Success/Error

            ---------------
            -- Set_Error --
            ---------------

            procedure Set_Error is
            begin
               Success := False;
               Error := +"invalid case expression";
            end Set_Error;

         begin
            if Value.Kind /= TOML.TOML_Table then
               return False;
            end if;

            for E of Value.Iterate_On_Table loop
               --  If we already detected an error, do nothing more. If we
               --  determined so far that this is a case expression, then we
               --  now know it's invalid as there can be only one field in this
               --  case.

               if not Success then
                  exit;
               elsif Is_Case_TOML = True then
                  Set_Error;
                  exit;
               end if;

               declare
                  Name      : constant String := +E.Key;
                  Var_First : constant Positive :=
                     Name'First + Case_Prefix'Length;
                  Var_Last  : constant Natural :=
                     Name'Last - Case_Suffix'Length;
               begin
                  --  Determine if this is a key for a case expression

                  if not Alire.Utils.Starts_With (Name, Case_Prefix) then
                     Is_Case_TOML := False;
                     exit;
                  end if;

                  --  Name definitely looks like part of a case expression. If
                  --  we already decided this was not a case expression, then
                  --  we have an inconsistency, which is an error. Otherwise,
                  --  make sure it's a valid clause.

                  if Is_Case_TOML = False
                     or else not Alire.Utils.Ends_With (Name, Case_Suffix)
                     or else not Is_Valid_Variable_Name
                        (Name (Var_First ..  Var_Last))
                     or else E.Value.Kind /= TOML.TOML_Table
                  then
                     Set_Error;
                     exit;
                  end if;

                  Variable := +Name (Var_First .. Var_Last);
                  Mappings := E.Value;
                  Is_Case_TOML := True;
               end;
            end loop;

            return (case Is_Case_TOML is
                    when Unknown => False,
                    when False   => False,
                    when True    => True);
         end Is_Case_TOML;

         ----------------
         -- Parse Node --
         ----------------

         function Parse_Node (Value : TOML.TOML_Value) return Expression_Node
         is
            Variable : US.Unbounded_String;
            Mappings : TOML.TOML_Value;
            Node     : Expression_Node;

            Value_Set : String_Sets.Set;
            --  Set of values for the controlling variables that this case must
            --  cover.

            Position : Matcher_Maps.Cursor;
         begin
            --  If TOML is a case expression pattern, convert it to the
            --  corresponding expression tree.

            if Is_Case_TOML (Value, Variable, Mappings) then
               Node := new Expression_Node_Record'
                 (Is_Case  => True,
                  Variable => Variable,
                  Matchers => <>,
                  Default  => <>);
               Nodes.Append (Node);

               Value_Set := Env.Value_Sets.Element (Variable).all;

               --  Process all clauses in this case construct

               for E of Mappings.Iterate_On_Table loop
                  Parse_Clause (+E.Key, E.Value, Node.Matchers, Node.Default);
                  if not Success then
                     return null;
                  end if;
               end loop;

               --  If no default clause was present and we are asked to provide
               --  automatically a default one, do it.

               if Node.Default = null and then Default.Present then
                  Node.Default := new Expression_Node_Record'
                    (Is_Case => False, Literal => Default.Value);
                  Nodes.Append (Node.Default);
               end if;

               --  Make sure that all matching values are valid values for this
               --  variable.

               Position := Node.Matchers.First;
               while Matcher_Maps.Has_Element (Position) loop
                  declare
                     Key : constant US.Unbounded_String :=
                        Matcher_Maps.Key (Position);
                  begin
                     if not Value_Set.Contains (Key) then
                        Success := False;
                        Error := +("invalid value for " & (+Variable)
                                   & ": " & (+Key));
                        return null;
                     else
                        Value_Set.Delete (Key);
                     end if;
                  end;
                  Position := Matcher_Maps.Next (Position);
               end loop;

               --  Make sure that all possible values for this variable have
               --  matchers.

               if Node.Default = null and then not Value_Set.Is_Empty then
                  Success := False;
                  Error := +("unhandled values for " & (+Variable) & ":");
                  for Value of Value_Set loop
                     US.Append (Error, " ");
                     US.Append (Error, Value);
                  end loop;
                  return null;
               end if;

               return Node;

            elsif not Success then
               return null;
            end if;

            --  Otherwise, Value must be a leaf node for a literal

            declare
               Literal_Result : Evaluation_Result.T;
            begin
               Parse_Literal (Value, Literal_Result);
               if Literal_Result.Success then
                  Node := new Expression_Node_Record'
                    (Is_Case => False,
                     Literal => Literal_Result.Value);
                  Nodes.Append (Node);
                  return Node;
               else
                  Success := False;
                  Error := Literal_Result.Error;
                  return null;
               end if;
            end;
         end Parse_Node;

         ------------------
         -- Parse_Clause --
         ------------------

         procedure Parse_Clause
           (Values   : String;
            Expr     : TOML.TOML_Value;
            Matchers : in out Matcher_Maps.Map;
            Default  : in out Expression_Node)
         is
            Node : Expression_Node;
            --  Expression node corresponding to the decoding of Expr

            function Process_Value (Value : String) return Boolean;
            --  Insert a matcher entry for Value/Node. Register an error if an
            --  entry is already registered for this value and return False.
            --  Otherwise return True.

            -------------------
            -- Process_Value --
            -------------------

            function Process_Value (Value : String) return Boolean
            is
               Position : Matcher_Maps.Cursor;
               Inserted : Boolean;
            begin
               if Value = "..." then
                  --  This clause matches all remaining values for the
                  --  controlling variable

                  if Default = null then
                     Default := Node;
                  else
                     Success := False;
                  end if;

               else
                  Matchers.Insert (+Value, Node, Position, Inserted);
                  if not Inserted then
                     Success := False;
                  end if;
               end if;

               if not Success then
                  Error := +("duplicate entry: " & Value);
               end if;
               return Success;
            end Process_Value;

            Dummy : Boolean;

         begin
            if not Success then
               return;
            end if;

            --  First, make sure that Expr is a valid expression

            Node := Parse_Node (Expr);
            if not Success then
               return;
            end if;

            --  Then, go through all values in Values (|-separated list) and
            --  register matchers for them.

            Dummy := Process_Case_Clause (Values, Process_Value'Access);
         end Parse_Clause;

         Root : constant Expression_Node := Parse_Node (Value);
      begin
         --  If there was an error during the parsing, make sure we free all
         --  nodes before returning.

         if Success then
            return
              (Success => True,
               Value => (Ada.Finalization.Limited_Controlled with
                         Root  => Root,
                         Nodes => Nodes));

         else
            for Node of Nodes loop
               Free (Node);
            end loop;
            return (Success => False, Error => Error);
         end if;
      end Parse;

      --------------
      -- Evaluate --
      --------------

      function Evaluate
        (Expr : Expression;
         Env  : Environment_Variables) return Evaluation_Result.T
      is
         Result : Evaluation_Result.T := (Success => True, others => <>);

         procedure Set_Error (Message : String);
         --  Set Result to an error with the given Message

         ---------------
         -- Set_Error --
         ---------------

         procedure Set_Error (Message : String) is
         begin
            Result := (Success => False, Error => +Message);
         end Set_Error;

         Node : Expression_Node := Expr.Root;
      begin
         --  Look for the leaf expression node corresponding to values in the
         --  environment.

         while Node.Is_Case loop
            declare
               Env_Value : US.Unbounded_String;

               Matcher_Cur : Matcher_Maps.Cursor;
            begin
               --  Lookup the value for the case controlling variable

               if Variable_Defined (Env, Node.Variable) then
                  Env_Value := Variable_Value (Env, Node.Variable);
               else
                  Set_Error ("undefined variable: " & (+Node.Variable));
                  return Result;
               end if;

               --  Find the next expression to evaluate: either the
               --  expression from the corresponding matcher or a default
               --  expression.

               Matcher_Cur := Node.Matchers.Find (Env_Value);
               if Matcher_Maps.Has_Element (Matcher_Cur) then
                  Node := Matcher_Maps.Element (Matcher_Cur);
               elsif Node.Default /= null then
                  Node := Node.Default;
               else
                  Set_Error ("unhandled value for " & (+Node.Variable)
                             & ": " & (+Env_Value));
                  return Result;
               end if;
            end;
         end loop;

         Result.Value := Node.Literal;
         return Result;
      end Evaluate;

      -------------------------
      -- Evaluate_Or_Default --
      -------------------------

      function Evaluate_Or_Default
        (Expr    : Expression;
         Default : Value_Type;
         Env     : Environment_Variables) return Evaluation_Result.T is
      begin
         return (if Expr.Root = null
                 then (Success => True, Value => Default)
                 else Evaluate (Expr, Env));
      end Evaluate_Or_Default;

      ----------
      -- Move --
      ----------

      procedure Move
        (Destination : out Expression; Source : in out Expression)
      is
         Root : constant Expression_Node := Source.Root;
      begin
         Source.Root := null;
         Destination.Nodes.Move (Source.Nodes);
         Destination.Root := Root;
      end Move;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Self : in out Expression) is
      begin
         for N of Self.Nodes loop
            Free (N);
         end loop;
      end Finalize;

   end Single_Values;

   package body Composite_Values is

      -----------
      -- Parse --
      -----------

      function Parse
        (Value : TOML.TOML_Value;
         Env   : Environment_Variables) return Parsing_Result
      is

         Success : Boolean := True;
         Error   : US.Unbounded_String;

         procedure Set_Error (Message : String);
         --  Assign an error to Success/Error

         Nodes : Node_Vectors.Vector;
         --  List of nodes we allocate in this function. This gives a simple
         --  method to free memory in case of error.

         function Cases_Count (Value : TOML.TOML_Value) return Natural
            with Pre => Success;
         --  Return the numbero of case constructs in the Value mapping. If
         --  Value is not an object, call Set_Error. Note that although this
         --  does not check that case constructs are valid, is it safe to call
         --  this on any TOML value.

         function Parse_Case_Key
           (Key       : String;
            Var_First : out Positive;
            Var_Last  : out Natural) return Boolean
            with Pre => Success;
         --  Parse a case construct key, for instance: "case(os)". On invalid
         --  syntax, just return false. Otherwise, set Var_First and Var_Last
         --  to the bounds for the "os" substring in the above example.

         function Parse_Node (Value : TOML.TOML_Value) return Expression_Node
            with Pre => Success;
         --  Parse an expression node. We expect Value to be an object that
         --  contains a mix of case constructs, to be processed by
         --  Parse_Matchers, and of entries. These entries will be passed to
         --  Parse_Literal.

         ---------------
         -- Set_Error --
         ---------------

         procedure Set_Error (Message : String) is
         begin
            Success := False;
            Error := +Message;
         end Set_Error;

         -----------------
         -- Cases_Count --
         -----------------

         function Cases_Count (Value : TOML.TOML_Value) return Natural is
            Result : Natural := 0;
         begin
            if Value.Kind /= TOML.TOML_Table then
               Set_Error ("object expected");
               return 0;
            end if;

            for Key of Value.Keys loop
               if Alire.Utils.Starts_With (+Key, Case_Prefix) then
                  Result := Result + 1;
               end if;
            end loop;
            return Result;
         end Cases_Count;

         --------------------
         -- Parse_Case_Key --
         --------------------

         function Parse_Case_Key
           (Key       : String;
            Var_First : out Positive;
            Var_Last  : out Natural) return Boolean is
         begin
            --  We have a valid key as soon as we have the expected suffix and
            --  prefix and that the name in between is a valid variable name.

            if not Alire.Utils.Starts_With (Key, Case_Prefix)
               or not Alire.Utils.Ends_With (Key, Case_Suffix)
            then
               return False;
            end if;

            Var_First := Key'First + Case_Prefix'Length;
            Var_Last := Key'Last - Case_Suffix'Length;

            return Is_Valid_Variable_Name (Key (Var_First .. Var_Last));
         end Parse_Case_Key;

         ----------------
         -- Parse_Node --
         ----------------

         function Parse_Node (Value : TOML.TOML_Value) return Expression_Node
         is
            Expr : Expression_Node;
            --  Expression to build

            Size : constant Natural := Cases_Count (Value);
            --  Number of cases in Expr

            Next_Case : Positive := 1;
            --  Index of the next couple variable/case to fill in Expr

            Base : constant TOML.TOML_Value := Value.Clone;
            --  Copy of Value. Process_Entry will remove case constructs from
            --  it, so after the iteration, it should contain a valid literal.

            procedure Parse_Clause
              (Values : String; Subexpr : TOML.TOML_Value);
            --  Parser to be called on all associations inside a case
            --  expression.  Name contains the various values to match (A|B for
            --  A or B) and Value is the expression associated. The
            --  corresponding entries are inserted into Matchers.
            --
            --  This assigns an error to Success/Error if anything invalid
            --  happens, and does nothing in the first place if Success is
            --  false.

            ------------------
            -- Parse_Clause --
            ------------------

            procedure Parse_Clause (Values : String; Subexpr : TOML.TOML_Value)
            is
               Node     : Expression_Node;
               Matchers : Matcher_Maps.Map renames Expr.Cases (Next_Case);
               Default  : Expression_Node renames Expr.Defaults (Next_Case);

               function Process_Value (Value : String) return Boolean;
               --  Insert a matcher entry for Value/Node. Register an error if
               --  an entry is already registered for this value and return
               --  False.  Otherwise return True.

               -------------------
               -- Process_Value --
               -------------------

               function Process_Value (Value : String) return Boolean
               is
                  Position : Matcher_Maps.Cursor;
                  Inserted : Boolean;
               begin
                  if Value = "..." then
                     --  This clause matches all remaining values for the
                     --  controlling variable

                     if Default = null then
                        Default := Node;
                     else
                        Success := False;
                     end if;

                  else
                     Matchers.Insert (+Value, Node, Position, Inserted);
                     if not Inserted then
                        Success := False;
                     end if;
                  end if;

                  if not Success then
                     Error := +("duplicate entry: " & Value);
                  end if;
                  return Success;
               end Process_Value;

            Dummy : Boolean;
            begin
               if not Success then
                  return;
               end if;

               --  First, make sure that Expr is a valid expression

               Node := Parse_Node (Subexpr);
               if not Success then
                  return;
               end if;

               --  Then, go through all values in Values (|-separated list) and
               --  register matchers for them.

               Dummy := Process_Case_Clause (Values, Process_Value'Access);
            end Parse_Clause;

         begin
            if not Success then
               return null;
            end if;

            Expr := new Expression_Node_Record (Size);
            Nodes.Append (Expr);

            --  Process case constructs in Value

            for Case_Construct of Value.Iterate_On_Table loop
               declare
                  Key       : constant String := +Case_Construct.Key;
                  Var_First : Positive;
                  Var_Last  : Natural;

                  Value_Set : String_Sets.Set;
                  --  Set of values for the controlling variables that this
                  --  case must cover.

                  Position : Matcher_Maps.Cursor;
               begin
                  --  This pass only reacts to case construct entries

                  if Alire.Utils.Starts_With (Key, Case_Prefix) then
                     if not Parse_Case_Key (Key, Var_First, Var_Last)
                        or else Case_Construct.Value.Kind /= TOML.TOML_Table
                     then
                        Set_Error ("invalid case construct");
                        return null;
                     end if;

                     Expr.Variables (Next_Case) :=
                        +Key (Var_First .. Var_Last);

                     Value_Set := Env.Value_Sets.Element
                       (Expr.Variables (Next_Case)).all;

                     --  Process all clauses in this case construct

                     for Case_Entry of Case_Construct.Value.Iterate_On_Table
                     loop
                        Parse_Clause (+Case_Entry.Key, Case_Entry.Value);
                        if not Success then
                           return null;
                        end if;
                     end loop;

                     --  Make sure that all matching values are valid values
                     --  for this variable.

                     Position := Expr.Cases (Next_Case).First;
                     while Matcher_Maps.Has_Element (Position) loop
                        declare
                           Key : constant US.Unbounded_String :=
                              Matcher_Maps.Key (Position);
                        begin
                           if not Value_Set.Contains (Key) then
                              Success := False;
                              Error := +("invalid value for "
                                         & (+Expr.Variables (Next_Case))
                                         & ": " & (+Key));
                              return null;
                           else
                              Value_Set.Delete (Key);
                           end if;
                        end;
                        Position := Matcher_Maps.Next (Position);
                     end loop;

                     --  Make sure that all possible values for this variable
                     --  have matchers.

                     if Expr.Defaults (Next_Case) = null
                        and then not Value_Set.Is_Empty
                     then
                        Success := False;
                        Error := +("unhandled values for "
                                   & (+Expr.Variables (Next_Case)) & ":");
                        for Value of Value_Set loop
                           US.Append (Error, " ");
                           US.Append (Error, Value);
                        end loop;
                        return null;
                     end if;
                  end if;
               end;

               --  Remove this case construct from Base and prepare the next
               --  pass.

               Base.Unset (Case_Construct.Key);
               Next_Case := Next_Case + 1;
            end loop;
            if not Success then
               return null;
            end if;

            --  The previous operation removed case constructs in place, so
            --  at this point, all we have left in Value is a simple value,
            --  ready to be parsed as a literal.

            declare
               Res : Evaluation_Result.T;
            begin
               Parse_Literal (Base, Res);
               if Res.Success then
                  Expr.Base_Value := Res.Value;
                  return Expr;
               else
                  Success := False;
                  Error := Res.Error;
                  return null;
               end if;
            end;
         end Parse_Node;

         Root : constant Expression_Node := Parse_Node (Value);
      begin
         --  If there was an error during the parsing, make sure we free all
         --  nodes before returning.

         if Success then
            return
              (Success => True,
               Value => (Ada.Finalization.Limited_Controlled with
                         Root  => Root,
                         Nodes => Nodes));

         else
            for Node of Nodes loop
               Free (Node);
            end loop;
            return (Success => False, Error => Error);
         end if;
      end Parse;

      --------------
      -- Evaluate --
      --------------

      function Evaluate
        (Expr : Expression;
         Env  : Environment_Variables) return Evaluation_Result.T
      is
         Result : Evaluation_Result.T := (Success => True, others => <>);

         procedure Set_Error (Message : String);
         --  Set Result to an error with the given Message

         procedure Evaluate (Node : Expression_Node);
         --  Recursive helper to evaluate a sub-expression

         ---------------
         -- Set_Error --
         ---------------

         procedure Set_Error (Message : String) is
         begin
            Result := (Success => False, Error => +Message);
         end Set_Error;

         --------------
         -- Evaluate --
         --------------

         procedure Evaluate (Node : Expression_Node) is
         begin
            if not Result.Success then
               return;
            end if;

            --  Merge the base value into the result

            declare
               Merged : Evaluation_Result.T;
            begin
               Merge (Result.Value, Node.Base_Value, Merged);
               Result := Merged;
               if not Merged.Success then
                  return;
               end if;
            end;

            --  Go through all nested case constructs, locate the clause
            --  corresponding to 

            for I in 1 .. Node.Case_Count loop
               declare
                  Variable : US.Unbounded_String renames Node.Variables (I);
                  Matchers : Matcher_Maps.Map renames Node.Cases (I);
                  Default  : Expression_Node renames Node.Defaults (I);

                  Env_Value : US.Unbounded_String;

                  Matcher_Cur : Matcher_Maps.Cursor;
               begin
                  --  Lookup the value for the case controlling variable

                  if Variable_Defined (Env, Variable) then
                     Env_Value := Variable_Value (Env, Variable);
                  else
                     Set_Error ("undefined variable: " & (+Variable));
                     return;
                  end if;

                  --  Find the next expression to evaluate: either the
                  --  expression from the corresponding matcher or a default
                  --  expression.

                  Matcher_Cur := Matchers.Find (Env_Value);
                  if Matcher_Maps.Has_Element (Matcher_Cur) then
                     Evaluate (Matcher_Maps.Element (Matcher_Cur));
                  elsif Default /= null then
                     Evaluate (Default);
                  else
                     Set_Error ("unhandled value for " & (+Variable)
                                & ": " & (+Env_Value));
                     return;
                  end if;
               end;
            end loop;
         end Evaluate;

      begin
         Evaluate (Expr.Root);
         return Result;
      end Evaluate;

      -------------------------
      -- Evaluate_Or_Default --
      -------------------------

      function Evaluate_Or_Default
        (Expr    : Expression;
         Default : Value_Type;
         Env     : Environment_Variables) return Evaluation_Result.T is
      begin
         return (if Expr.Root = null
                 then (Success => True, Value => Default)
                 else Evaluate (Expr, Env));
      end Evaluate_Or_Default;

      ----------
      -- Move --
      ----------

      procedure Move
        (Destination : out Expression; Source : in out Expression)
      is
         Root : constant Expression_Node := Source.Root;
      begin
         Source.Root := null;
         Destination.Nodes.Move (Source.Nodes);
         Destination.Root := Root;
      end Move;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Self : in out Expression) is
      begin
         for N of Self.Nodes loop
            Free (N);
         end loop;
      end Finalize;

   end Composite_Values;

end Alire.TOML_Expressions;
