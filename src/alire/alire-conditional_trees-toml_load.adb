with Alire.Conditional_Trees.Case_Nodes;
with Alire.TOML_Keys;

package body Alire.Conditional_Trees.TOML_Load is

   Others_Key : String renames TOML_Keys.Case_Others;

   package Case_Nodes is new Conditional_Trees.Case_Nodes;

   ----------
   -- Load --
   ----------

   function Load (From    : TOML_Adapters.Key_Queue;
                  Loader  : not null Static_Loader;
                  Resolve : Boolean;
                  Strict  : Boolean)
                  return Tree
   is
      use TOML;

      ------------------
      -- Process_Case --
      ------------------
      --  Resolve a single case expression, creating a case node. For the
      --  leaves, it may recursively call Load.
      function Process_Case (Parent   : TOML_Adapters.Key_Queue;
                             Root_Key : String;
                             Case_Key : String;
                             Case_Val : TOML_Value)
                             return Tree
      is
         use Utils;

         ---------------------
         -- Process_Entries --
         ---------------------

         function Process_Entries (Case_Table : TOML_Adapters.Key_Queue)
                                   return Tree
         is
            Var : constant Expressions.Variable :=
                    Expressions.From
                      (Key => Head (Tail (Case_Key, '('), ')'));
            Map : Case_Nodes.Map := Case_Nodes.Case_Maps.Empty (Var);
            --  The map only accepts values matching the Variable
         begin
            loop
               declare
                  Item_Val : TOML_Value;
                  Item_Key : constant String := Case_Table.Pop (Item_Val);
               begin
                  exit when Item_Key = "";

                  --  Do an initial vetting before loading

                  if Item_Key /= Others_Key and then
                    not Map.Base.Is_Valid (Item_Key)
                  then
                     if Strict then
                        Case_Table.Recoverable_Error
                          ("invalid enumeration value: " & Item_Key);
                     else
                        Trace.Debug
                          (Case_Table.Message
                             ("unknown enumeration value: " & Item_Key));
                     end if;
                  end if;

                  --  Load the value and assign to the appropriate entry

                  Map.Insert
                    (Item_Key,
                     Load -- recursively load the item for this entry
                       (From    => Case_Table.Descend
                            (Key     => Root_Key,
                             Value   => Item_Val,
                             Context => Item_Key),
                        Loader  => Loader,
                        Resolve => Resolve,
                        Strict  => Strict));
               end;
            end loop;

            return Case_Nodes.New_Case (Map);
         end Process_Entries;

      begin
         if Starts_With (Case_Key, "case(") and then
           Case_Key (Case_Key'Last) = ')'
         then
            return Process_Entries (Parent.Descend (Case_Val, Case_Key));
         else
            Parent.Checked_Error ("'case(..)' expected; got: " & Case_Key);
         end if;
      end Process_Case;

      --------------------------
      -- Process_Nested_Table --
      --------------------------
      --  Val is a table that holds either values to be directly loaded by the
      --  static loader, or expressions to be resolved by us prior to loading.
      function Process_Nested_Table (Key : String; Val : TOML_Value)
                                     return Tree
      is
         Table : constant TOML_Adapters.Key_Queue :=
                   From.Descend (Val, "values");
      begin
         return Result : Tree do

            --  We need to pop and resolve expressions, and send all the
            --  remaining keys together to the static loader.

            loop
               declare
                  Case_Val : TOML_Value;
                  Case_Key : constant String :=
                               Table.Pop_Expr ("case(", Case_Val);
               begin
                  exit when Case_Key = ""; -- Table contains no more cases

                  Result.Append
                    (Process_Case (From, Key, Case_Key, Case_Val));
               end;
            end loop;

            --  Finally, process remaining contents as a single static value

            if Val.Keys'Length > 0 then
               Result.Append
                 (Loader
                    (From.Descend
                         (Key     => Key,
                          Value   => Val,
                          Context => Key)));
            end if;
         end return;
      end Process_Nested_Table;

   begin

      --  Ensure only one top-level value provided

      From.Assert
        (From.Unwrap.Kind = TOML_Table,
         "Expected a table but got: " & From.Unwrap.Kind'Image);

      From.Assert
        (From.Unwrap.Keys'Length = 1,
         "Expected a single entry in table, but got:"
         & From.Unwrap.Keys'Length'Image);

      --  Get the key = ... and process it

      declare
         Val : TOML_Value;
         Key : constant String := From.Pop (Val);
      begin
         --  Val might be a dynamic expression, or a value to be processed by
         --  the static loader. If the value isn't a table, certainly it isn't
         --  an expression.

         if Val.Kind /= TOML_Table then
            return Loader (From.Descend
                           (Key     => Key,
                            Value   => Val,
                            Context => Key));
         else
            --  See what the Val table holds
            return Process_Nested_Table (Key, Val);
         end if;
      end;
   end Load;

end Alire.Conditional_Trees.TOML_Load;
