with Alire.TOML_Adapters;

with GNAT.IO;

package body Alire.Conditional_Trees.Case_Nodes is

   type Case_Node is new Node with record
      Cases : Map;
   end record;

   overriding
   function Evaluate (This    : Case_Node;
                      Against : Properties.Vector)
                      return Tree'Class;

   overriding
   function Flatten (This : Case_Node) return Node'Class;

   overriding
   function Image (This : Case_Node) return String;

   overriding
   function Leaf_Count (This : Case_Node) return Positive;

   overriding
   procedure Print (This    : Case_Node;
                    Prefix  : String;
                    Verbose : Boolean;
                    Sorted  : Boolean);

   overriding
   procedure To_TOML (This : Case_Node; Parent : TOML.TOML_Value);

   overriding
   function To_YAML (This : Case_Node) return String;

   ------------------
   -- Contains_ORs --
   ------------------

   overriding
   function Contains_ORs (This : Case_Node) return Boolean is (False);

   --------------------
   -- Is_Conditional --
   --------------------

   overriding
   function Is_Conditional (This : Case_Node) return Boolean is (True);

   --------------------
   -- Image_One_Line --
   --------------------

   function Image_One_Line (This : Map) return String is
      Result : UString;
      use UStrings;

      Keys   : constant Case_Maps.Key_Array :=
                 This.Keys (Ada_Like => True, Exclude_Others => False);
   begin
      for I in Keys'Range loop
         Append (Result,
                 String'(
                 "when " & TOML_Adapters.Adafy (+Keys (I))
                   & " => " & This.Element (+Keys (I)).Image_One_Line));
         if I /= Keys'Last then
            Append (Result, ", ");
         end if;
      end loop;

      return +Result;
   end Image_One_Line;

   -----------
   -- Image --
   -----------

   overriding
   function Image (This : Case_Node) return String is
     ("(case " & This.Cases.Base.Name & " is "
      & Image_One_Line (This.Cases) & ")");

   -----------
   -- Print --
   -----------

   overriding
   procedure Print (This    : Case_Node;
                    Prefix  : String;
                    Verbose : Boolean;
                    Sorted  : Boolean)
   is
      use GNAT.IO;
      Tab : constant String := "   ";
   begin
      Put_Line (Prefix & "case " & This.Cases.Base.Name & " is");
      for Key of This.Cases.Keys (Ada_Like => True, Exclude_Others => False)
      loop
         Put_Line (Prefix & Tab & "when "
                   & Utils.To_Mixed_Case (+Key) & " => "
                   & (if not Verbose
                     then This.Cases.Element (+Key).Image_One_Line
                     else ""));
         if Verbose then
            Print (This.Cases.Element (+Key),
                   Prefix & Tab & Tab, Verbose, Sorted);
         end if;
      end loop;
   end Print;

   --------------
   -- Evaluate --
   --------------

   overriding
   function Evaluate (This    : Case_Node;
                      Against : Properties.Vector)
                      return Tree'Class is
   begin
      return Eval : Tree := Empty do
         for Prop of Against loop
            for Value of This.Cases.Keys (Ada_Like       => False,
                                          Exclude_Others => True) loop
               if Expressions.Satisfies (Property => Prop,
                                         Var_Key  => This.Cases.Base.Key,
                                         Value    => +Value)
               then
                  Eval.Append (This.Cases.Element (+Value).Evaluate (Against));
               end if;
            end loop;
         end loop;
      end return;
   end Evaluate;

   ----------------
   -- Leaf_Count --
   ----------------

   overriding
   function Leaf_Count (This : Case_Node) return Positive
   is
      function Count (This : Tree) return Natural renames Leaf_Count;
   begin
      return This.Cases.Size (Count'Access);
   end Leaf_Count;

   -------------
   -- Flatten --
   -------------

   overriding
   function Flatten (This : Case_Node) return Node'Class is
      Flat : Tree;
   begin
      for Key of This.Cases.Keys (Exclude_Others => True, Ada_Like => False)
      loop
         if not This.Cases.Element (+Key).Is_Empty then
            Flat := Flat and To_Tree (This.Cases.Element (+Key).Root.Flatten);
         end if;
      end loop;

      if This.Cases.Has_Others then
         if not This.Cases.Other.Is_Empty then
            Flat := Flat and To_Tree (This.Cases.Other.Root.Flatten);
         end if;
      end if;

      return Flat.Root;
   end Flatten;

   -------------
   -- To_TOML --
   -------------

   overriding
   procedure To_TOML (This : Case_Node; Parent : TOML.TOML_Value) is
   begin
      raise Unimplemented;
      --  Not yet needed, unless we implement full-fledged case exports
   end To_TOML;

   -------------
   -- To_YAML --
   -------------

   overriding
   function To_YAML (This : Case_Node) return String is
   begin
      raise Unimplemented;
      --  Not yet needed, unless we implement full-fledged case exports
      return "";
   end To_YAML;

   --------------
   -- New_Case --
   --------------

   function New_Case (Cases : Map) return Tree
   is (To_Tree (Case_Node'(Cases => Cases)));

end Alire.Conditional_Trees.Case_Nodes;
