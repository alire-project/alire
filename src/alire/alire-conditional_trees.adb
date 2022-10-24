with Ada.Containers.Indefinite_Ordered_Maps;

with Alire.TOML_Adapters;

package body Alire.Conditional_Trees is

   Tab : constant String := "   ";

   ---------------------
   -- Image_Classwide --
   ---------------------

   function Image_Classwide (This : Node'Class) return String is (This.Image);

   -------------
   -- To_YAML --
   -------------

   overriding
   function To_YAML (This : Tree) return String
   is (if This.Is_Empty
       then ""
       else This.Element.To_YAML);

   overriding
   function To_YAML (V : Leaf_Node) return String is
     (V.Value.Constant_Reference.To_YAML);

   overriding
   function To_YAML (V : Vector_Node) return String is
     (Non_Primitive.To_YAML (V.Values));

   -----------
   -- Image --
   -----------

   overriding function Image (V : Leaf_Node) return String is
     (Image (V.Value.Constant_Reference));

   -----------------
   -- Conjunction --
   -----------------

   function Conjunction (This : Vector_Node) return Conjunctions is
     (This.Conjunction);

   -----------
   -- Image --
   -----------

   overriding function Image (V : Vector_Node) return String is
     ("(" & (if V.Conjunction = Anded
             then Non_Primitive.One_Liner_And (V.Values)
             else Non_Primitive.One_Liner_Or (V.Values)) & ")");

   -----------------
   -- Conjunction --
   -----------------

   function Conjunction (This : Tree) return Conjunctions is
     (Vector_Node'Class (This.Element).Conjunction);

   -----------------
   -- First_Child --
   -----------------

   function First_Child (This : Tree) return Tree is
     (if This.Is_Value
      then This
      else To_Tree (Vector_Node (This.Root).Values.First_Element));

   --------------
   -- New_Leaf --
   --------------

   function New_Leaf (V : Values) return Tree is
     (To_Holder (Leaf_Node'(Value => Definite_Values.To_Holder (V))));

   -----------
   -- Empty --
   -----------

   function Empty return Tree is
      (Holders.Empty_Holder with null record);

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty (This : Tree) return Boolean is
     (Holders.Holder (This).Is_Empty);

   --------------------
   -- Image_One_Line --
   --------------------

   function Image_One_Line (This : Tree) return String is
     (if This.Is_Empty
      then "(empty)"
      else This.Element.Image);

   ----------------------------
   -- All_But_First_Children --
   ----------------------------

   function All_But_First_Children (This : Tree) return Tree is
   begin
      if This.Is_Value then
         return Empty;
      else
         declare
            Children : Vectors.Vector := Vector_Node (This.Root).Values;
         begin
            Children.Delete_First;
            return To_Holder (Vector_Node'(This.Conjunction, Children));
         end;
      end if;
   end All_But_First_Children;

   -------------
   -- Flatten --
   -------------
   --  Remove redundant and/or subtrees by merging upwards.
   procedure Flatten (Inner : in out Vector_Node;
                      This  : Node'Class;
                      Conj  : Conjunctions)
   is
   begin
      if This in Leaf_Node then
         Inner.Values.Append (This);
      elsif This in Vector_Node then
         --  Flatten only if conjunction matches, otherwise just append
         --  subtree.
         if Vector_Node (This).Conjunction = Conj then
            for Child of Vector_Node (This).Values loop
               Flatten (Inner, Child, Conj);
            end loop;
         else
            Inner.Values.Append (This);
         end if;
      else
         --  Unknown node class, just append subtree:
         Inner.Values.Append (This);
      end if;
   end Flatten;

   -------------
   -- Flatten --
   -------------

   overriding
   function Flatten (This : Vector_Node) return Tree'Class is
      Result : Tree;
   begin
      for Child of This.Values loop
         case This.Conjunction is
            when Anded => Result := Result and Tree (Child.Flatten);
            when Ored =>  Result := Result or  Tree (Child.Flatten);
         end case;
      end loop;

      return Result;
   end Flatten;

   ----------
   -- Join --
   ----------

   function Join (L, R : Tree; Op : Conjunctions) return Tree is
      Inner : Vector_Node := (Conjunction => Op, Values => <>);
   begin
      if not L.Is_Empty then
         Flatten (Inner, L.Constant_Reference, Op);
      end if;

      if not R.Is_Empty then
         Flatten (Inner, R.Constant_Reference, Op);
      end if;

      if Inner.Values.Is_Empty then
         return Empty;
      else

         --  Convert vector with single value into value

         if Inner.Values.Length in 1 then
            return Inner.Values.First_Element.To_Tree;
         end if;

         return To_Holder (Inner);
      end if;
   end Join;

   -----------
   -- "and" --
   -----------

   function "and" (L, R : Tree) return Tree
   is (Join (L, R, Anded));

   ----------
   -- "or" --
   ----------

   function "or" (L, R : Tree) return Tree
   is (Join (L, R, Ored));

   ------------
   -- Append --
   ------------

   procedure Append (L : in out Tree; R : Tree) is
   begin
      L := L and R;
   end Append;

   ----------------
   -- Leaf_Count --
   ----------------

   overriding
   function Leaf_Count (This : Vector_Node) return Positive is
      Count : Natural := 0;
   begin
      for Child of This.Values loop
         Count := Count + Child.Leaf_Count;
      end loop;
      return Count;
   end Leaf_Count;

   -----------------
   -- Materialize --
   -----------------

   function Materialize (This : Tree;
                         Against : Properties.Vector)
                         return Collection
   is
      Col : Collection;
      Pre : constant Tree := This.Evaluate (Against);

      procedure Visit (Inner : Node'Class) is
      begin
         if Inner in Leaf_Node then
            Append (Col, Leaf_Node (Inner).Value.Constant_Reference);
         elsif Inner in Vector_Node then
            if Vector_Node (Inner).Conjunction = Anded then
               for Child of Vector_Node (Inner).Values loop
                  Visit (Child);
               end loop;
            else
               raise Constraint_Error
                 with "OR trees cannot be materialized as list";
            end if;
         elsif Inner.Is_Conditional then
            raise Program_Error with
              "No conditional nodes should remain after tree evaluation";
         else
            raise Program_Error with
              "Unconditional node of unknown class";
         end if;
      end Visit;

   begin
      if not Pre.Is_Empty then
         Visit (Pre.Constant_Reference);
      end if;
      return Col;
   end Materialize;

   ---------------
   -- Enumerate --
   ---------------

   function Enumerate (This : Tree) return Collection is
      Col : Collection with Warnings => Off;
      Flat : constant Tree :=
               (if This.Is_Empty
                then This
                else Tree (This.Constant_Reference.Flatten));
   begin
      if not Flat.Is_Empty then
         if Flat.Constant_Reference in Leaf_Node then
            Append (Col, Leaf_Node (Flat.Constant_Reference.Element.all)
                         .Value.Constant_Reference);
         elsif Flat.Constant_Reference in Vector_Node then
            for Child of Vector_Node (Flat.Constant_Reference.Element.all)
                         .Values
            loop
               Append (Col, Leaf_Node (Child).Value.Constant_Reference);
            end loop;
         else
            raise Program_Error with
              "Flattened nodes must be leaves or vectors";
         end if;
      end if;

      return Col;
   end Enumerate;

   -------------
   -- As_List --
   -------------

   function As_List (This : Tree) return Value_Lists.List is
      function Enumerate is
        new Conditional_Trees.Enumerate (Value_Lists.List, Value_Lists.Append);
   begin
      return Enumerate (This);
   end As_List;

   --------------
   -- Evaluate --
   --------------

   overriding
   function Evaluate (This    : Vector_Node;
                      Against : Properties.Vector)
                      return Tree'Class
   is
      Result : Vector_Node;
   begin
      Result.Conjunction := This.Conjunction;
      for Child of This.Values loop
         declare
            Eval : constant Tree'Class := Child.Evaluate (Against);
         begin
            if not Eval.Is_Empty then
               Result.Values.Append (Eval.Root);
            end if;
         end;
      end loop;

      return Result.To_Tree and Empty;
      --  ANDing with empty ensures the vector is flattened. Cosmetic.
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (This : Tree; Against : Properties.Vector) return Tree is
   begin
      if This.Is_Empty then
         return This;
      else
         return Tree (This.Root.Evaluate (Against));
      end if;
   end Evaluate;

   ------------------
   -- Contains_ORs --
   ------------------

   function Contains_ORs (This : Tree) return Boolean is
      ((not This.Is_Empty) and then This.Root.Contains_ORs);

   ----------------------
   -- Is_Unconditional --
   ----------------------

   function Is_Unconditional (This : Tree) return Boolean is
      (This.Is_Empty or else not This.Root.Is_Conditional);

   ----------------------
   -- Iterate_Children --
   ----------------------

   procedure Iterate_Children (This    : Tree;
                               Visitor : access procedure (CV : Tree))
   is
   begin
      if not This.Is_Empty then
         if This.Is_Value then
            Visitor (This);
         elsif This.Is_Vector then
            for Inner of Vector_Node (This.Root).Values loop
               Visitor (Tree'(To_Holder (Inner)));
            end loop;
         else
            raise Constraint_Error with "Node is not a vector";
         end if;
      end if;
   end Iterate_Children;

   -----------
   -- Print --
   -----------

   overriding
   procedure Print (This    : Leaf_Node;
                    Prefix  : String;
                    Verbose : Boolean;
                    Sorted  : Boolean := False) is
      pragma Unreferenced (Verbose, Sorted);
   begin
      Trace.Always (Prefix & Image (This.Value.Constant_Reference));
   end Print;

   overriding
   procedure Print (This    : Vector_Node;
                    Prefix  : String;
                    Verbose : Boolean;
                    Sorted  : Boolean)
   is
      package Maps is new Ada.Containers.Indefinite_Ordered_Maps (String,
                                                                  Node'Class);
   begin
      if Verbose then
         case This.Conjunction is
            when Anded => Trace.Always (Prefix & "All of:");
            when Ored  => Trace.Always (Prefix & "First available of:");
         end case;
      end if;

      if Sorted then
         declare
            Map : Maps.Map;
         begin
            for Child of This.Values loop
               Map.Insert (Child.Image, Child);
            end loop;
            for Child of Map loop
               Print (Child,
                      Prefix & (if Verbose then Tab else ""),
                      Verbose, Sorted);
            end loop;
         end;
      else
         for Child of This.Values loop
            Print (Child,
                   Prefix & (if Verbose then Tab else ""),
                   Verbose, Sorted);
         end loop;
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (This    : Tree;
                    Prefix  : String  := "";
                    Verbose : Boolean := False;
                    And_Or  : Boolean := True;
                    Sorted  : Boolean := False) is
   begin
      if This.Is_Empty then
         Trace.Always (Prefix & "(empty)");
      else
         Print (This.Root, Prefix, And_Or or Verbose, Sorted);
      end if;
   end Print;

   -------------------
   -- Tree_TOML_Add --
   -------------------

   procedure Tree_TOML_Add (Table : TOML.TOML_Value;
                            Key   : String;
                            Val   : TOML.TOML_Value)
   is
      use AAA.Strings;

      --  Add one property to the parent table.
      --  Atomic values are automatically converted into arrays, if
      --    more than one for the same key appears (e.g., executables)
      --  Table values with same key are merged in a single table (e.g.,
      --  dependencies)
      --  Array values with same key are consolidated in a single array
      --    (e.g., actions, which are created as an array of tables).

   begin

      --  Get properties given as nested tables out of the way first, by
      --  getting the top-level key and using the nested table as the
      --  actual value.

      if (for some Char of Key => Char = '.') then
         Tree_TOML_Add (Table,
                        Key => Head (Key, '.'),
                        Val => TOML_Adapters.Create_Table
                          (Tail (Key, '.'), Val));
         return;
      end if;

      --  Regular processing of a top-level property

      pragma Assert (Table.Kind = TOML.TOML_Table);
      if Table.Has (Key) then
         declare
            Current : constant TOML.TOML_Value := Table.Get (Key);
         begin
            case Current.Kind is
               when TOML_Table =>
                  Table.Set (Key, TOML_Adapters.Merge_Tables (Current, Val));
               when TOML_Array =>
                  case Val.Kind is
                     when TOML.Atom_Value_Kind | TOML.TOML_Table =>
                        Current.Append (Val);
                     when TOML.TOML_Array =>
                        --  Consolidate the array into one
                        for I in 1 .. Val.Length loop
                           Current.Append (Val.Item (I));
                        end loop;
                  end case;
               when TOML.Atom_Value_Kind => -- Convert to array
                  declare
                     Replace : constant TOML.TOML_Value :=
                                 TOML.Create_Array;
                  begin
                     Replace.Append (Current);
                     Replace.Append (Val);
                     Table.Set (Key, Replace);
                  end;
            end case;
         end;
      else
         Table.Set (Key, Val);
      end if;
   end Tree_TOML_Add;

   -------------
   -- To_TOML --
   -------------

   overriding
   procedure To_TOML (This : Leaf_Node; Parent : TOML.TOML_Value) is
   begin
      Tree_TOML_Add (Parent,
                     This.Value.Constant_Reference.Key,
                     This.Value.Constant_Reference.To_TOML);
   end To_TOML;

   overriding
   procedure To_TOML (This : Vector_Node; Parent : TOML.TOML_Value) is
   begin
      case This.Conjunction is
         when Anded => null;
         when Ored  => raise Unimplemented
              with "Not yet in index specification";
      end case;

      for Child of This.Values loop
         To_TOML (Child, Parent);
      end loop;
   end To_TOML;

   overriding
   function To_TOML (This : Tree) return TOML.TOML_Value is
      Root_Table : constant TOML.TOML_Value := TOML.Create_Table;
   begin
      if not This.Is_Empty then
         This.Root.To_TOML (Root_Table);
      end if;
      return Root_Table;
   end To_TOML;

   -----------------
   --  ITERATORS  --
   -----------------

   type Forward_Iterator is new Iterators.Forward_Iterator with record
      Children : Vectors.Vector;
   end record;

   -----------
   -- First --
   -----------

   overriding function First (Object : Forward_Iterator) return Cursor is
     (if Object.Children.Is_Empty
      then Cursor (Vectors.No_Element)
      else Cursor (Object.Children.First));

   ----------
   -- Next --
   ----------

   overriding function Next (This : Cursor) return Cursor is
      (Cursor (Vectors.Next (Vectors.Cursor (This))));

   ----------
   -- Next --
   ----------

   overriding function Next (Object   : Forward_Iterator;
                             Position : Cursor) return Cursor is
     (Next (Position));

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element (This : Cursor) return Boolean is
      (Vectors.Has_Element (Vectors.Cursor (This)));

   -------------
   -- Iterate --
   -------------

   function Iterate (Container : Tree)
                     return Iterators.Forward_Iterator'Class is
   begin
      if Container.Is_Empty then
         return Forward_Iterator'(others => <>);
      elsif Container.Constant_Reference in Leaf_Node then
         return Single : Forward_Iterator do
            Single.Children.Append (Container.Element);
         end return;
      elsif Container.Constant_Reference not in Vector_Node then
         raise Constraint_Error
           with "Cannot iterate over non-vector conditional value";
      end if;

      return Forward_Iterator'
        (Children =>
           Vector_Node (Container.Element).Values);
   end Iterate;

   ---------------------
   -- Indexed_Element --
   ---------------------

   function Indexed_Element (Container : Tree;
                             Pos       : Cursor)
                             return Tree is
     (Tree'(To_Holder (Element (Pos))));

   -------------------------
   -- Recursive_Traversal --
   -------------------------

   overriding
   procedure Recursive_Traversal
     (This  : in out Leaf_Node;
      Apply : access procedure (Value : in out Values))
   is
   begin
      Apply (This.Value.Reference);
   end Recursive_Traversal;

   -------------------------
   -- Recursive_Traversal --
   -------------------------

   overriding
   procedure Recursive_Traversal
     (This  : in out Vector_Node;
      Apply : access procedure (Value : in out Values))
   is
   begin
      for Node of This.Values loop
         Node.Recursive_Traversal (Apply);
      end loop;
   end Recursive_Traversal;

   ---------------
   -- Visit_All --
   ---------------

   procedure Visit_All
     (This  : in out Tree;
      Apply : access procedure (Value : in out Values))
   is
   begin
      if This.Is_Empty then
         return;
      else
         This.Reference.Recursive_Traversal (Apply);
      end if;
   end Visit_All;

end Alire.Conditional_Trees;
