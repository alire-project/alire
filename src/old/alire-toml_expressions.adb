with Alire.Errors;
with Alire.Utils;

package body Alire.TOML_Expressions is

   use all type TOML.Any_Value_Kind;

   -------------------------
   -- Contains_Expression --
   -------------------------

   function Contains_Expression (Value : TOML.TOML_Value) return Boolean is
     (Value.Kind = TOML_Table and then
        (for some Key of Value.Keys =>
              Utils.Starts_With (+Key, Case_Prefix)));

   ---------------
   -- Enum_Tree --
   ---------------

   package body Enum_Trees is

      generic
         type Case_Keys is (<>);
         with function Loaders (Key : Case_Keys) return Recursive_Case_Loader;
      function Get_Case (Selector : String;
                         Context  : String) return Recursive_Case_Loader;
      --  Identifies which loader corresponds to a case(xx) Selector.
      --  May raise Checked_Error.

      --------------
      -- Get_Case --
      --------------

      function Get_Case (Selector : String;
                         Context  : String) return Recursive_Case_Loader
      is
         use Utils;
         Case_Var : constant String := Tail (Head (Selector, ')'), '(');
         Case_Key : Case_Keys;
      begin
         if Utils.Starts_With (Selector, Case_Prefix) and then
           Selector (Selector'Last) = ')'
         then
            Case_Key := Case_Keys'Value (TOML_Adapters.Adafy (Case_Var));
            --  Converts a variable to the Case_Keys enumeration, hence it may
            --  fail (see exception handler below).

            return Loader : constant Recursive_Case_Loader :=
              Loaders (Case_Key)
            do
               if Loader = null then
                  raise Checked_Error with
                    Context & ": null loader for case argument: " & Case_Var;
               end if;
            end return;
         else
            raise Checked_Error with
              Errors.Set (Context & "'case(..)' expected; got: " & Selector);
         end if;
      exception
         when Constraint_Error =>
            raise Checked_Error with
              Errors.Set ("invalid case variable: " & Case_Var);
      end Get_Case;

      ----------
      -- Load --
      ----------

      function Load (Parent        : String;
                     From          : TOML_Adapters.Key_Queue;
                     Static_Loader : Static_Tree_Loader) return Tree is
         use TOML;
         function Find_Case is new Get_Case (Case_Keys, Loaders);
         Val : constant TOML.TOML_Value := From.Unwrap;
      begin
         return T : Tree do
            --  Identify and process case expressions in the table:
            loop
               declare
                  Case_Tab : TOML.TOML_Value;
                  Case_Key : constant String :=
                               From.Pop_Expr (Case_Prefix, Case_Tab);
               begin
                  exit when Case_Key = "";
                  Trace.Debug ("Master load (case): " & Case_Key);

                  --  A case table:
                  T := T and Find_Case (Case_Key, From.Message ("case"))
                    (Parent,
                     From.Descend (Case_Tab, Case_Key),
                     Static_Loader);
               end;
            end loop;

            --  Process remainder keys as static values:
            if Val.Kind /= TOML_Table or else Val.Keys'Length > 0 then
               Trace.Debug ("Master load (static)");
               T := T and Static_Loader
                 (From.Descend (Parent, From.Unwrap, "static"));
            end if;
         end return;
      end Load;

   end Enum_Trees;

   ----------------
   -- Enum_Cases --
   ----------------

   package body Enum_Cases is

      ----------------
      -- Load_Cases --
      ----------------

      function Load_Cases (From  : TOML_Adapters.Key_Queue) return TOML_Array
      is
         Cases : TOML_Array := (others => TOML.No_TOML_Value);
         --  Cases not seen need an empty default.

         Seen  : array (Enum) of Boolean := (others => False);
         --  Track case entries that have appeared.
         --  This is currently used for a debug trace; we might decide that
         --  we prefer not having missing cases, like in regular Ada.

         -----------------
         -- Reduce_Seen --
         -----------------

         --  This recursive function creates a single line saying the case
         --  entries missing in the case expression.
         function Reduce_Seen (I     : Enum := Enum'First;
                               Comma : Boolean := False)
                               return String is
           ((if not Seen (I)
             then (if Comma
                   then ", "
                   else "") & TOML_Adapters.Tomify (I'Img)
             else "") &
            (if I = Enum'Last
             then ""
             else Reduce_Seen (Enum'Succ (I), Comma or not Seen (I))));

         use TOML;

      begin
         --  Treat the "..." case first
         declare
            RHS : TOML.TOML_Value;
         begin
            if From.Pop (Dots, RHS) then
               Seen  := (others => True);
               Cases := (others => RHS);
            end if;
         end;

         --  Treat explicit cases
         loop
            declare
               RHS : TOML_Value;
               LHS : constant String := From.Pop (RHS);
            begin
               exit when LHS = "";
               --  We have the value, store it in all pertinent keys:
               for E_Str of Utils.String_Vector'(Utils.Split (LHS, '|'))
               loop
                  declare
                     E : Enum;
                  begin
                     E := Enum'Value (TOML_Adapters.Adafy (E_Str));
                     Seen (E) := True;
                     Cases (E) := RHS;
                  exception
                     when others =>
                        if Strict_Enums then
                           From.Recoverable_Error
                             ("invalid enumeration value: " & E_Str);
                        else
                           Trace.Debug
                             (From.Message
                                ("unknown enumeration value: " & E_Str));
                        end if;
                  end;
               end loop;
            end;
         end loop;

         if (for some E of Seen => E = False) then
            Trace.Debug
              (From.Message ("missing enumeration cases: " & Reduce_Seen));
         end if;

         return Cases;
      end Load_Cases;

   end Enum_Cases;

   -------------------
   -- Tree_Builders --
   -------------------

   package body Tree_Builders is

      ----------------
      -- Load_Cases --
      ----------------

      --  This function loads a case expression and then recursively
      --  redispatchs to load the TOML value for every case entry.

      function Load_Cases (Parent        : String;
                           From          : TOML_Adapters.Key_Queue;
                           Static_Loader : Trees.Static_Tree_Loader;
                           Unused_Marker : Integer := 0)
                           return Trees.Tree
      is
         TOML_Cases : constant Cases.TOML_Array := Cases.Load_Cases (From);
         Tree_Cases : Enum_Array;
      begin
         --  TODO: we are reloading all cases here, even when several of them
         --  may point to the same TOML value. For that reason (also) we need
         --  deep copies below, or otherwise the unsetting of already processed
         --  nested tables breaks things.
         --  TL;DR: there is an optimization opportunity here to only load
         --  actually different TOML values (in which case deep copies wouldn't
         --  be necessary any longer).
         for I in TOML_Cases'Range loop
            declare
               Key : constant String          := TOML_Adapters.Adafy (I'Img);
               Val : constant TOML.TOML_Value := TOML_Cases (I);
            begin
               if Val.Is_Present then
                  Trace.Debug ("Loading key: " & Parent
                               & " in case: " & Key
                               & " from a: "   & Val.Kind'Img);
                  Tree_Cases (I) :=
                    Load
                      (Parent,
                       From.Descend (TOML_Cases (I).Clone, From.Message (Key)),
                       Static_Loader);
               else
                  Trace.Debug ("Applying default to key: " & Parent
                               & " for missing case: " & Key);
                  Tree_Cases (I) := Trees.Default;
               end if;
            end;
         end loop;

         return New_Leaf (Tree_Cases);
      end Load_Cases;

   end Tree_Builders;

end Alire.TOML_Expressions;
