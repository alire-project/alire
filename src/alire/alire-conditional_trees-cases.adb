with GNAT.IO;

package body Alire.Conditional_Trees.Cases is

   use all type Enum;

   type Case_Node is new Node with record
      Cases : Cases_Array;
   end record;
   --  A new node class to store a case statement.

   overriding
   function Evaluate (This    : Case_Node;
                      Against : Properties.Vector)
                      return Tree'Class;

   overriding
   procedure To_TOML (This : Case_Node; Parent : TOML.TOML_Value);

   overriding
   function To_YAML (This : Case_Node) return String;

   overriding
   function Contains_ORs (This : Case_Node) return Boolean is (False);

   overriding
   function Flatten (This : Case_Node) return Node'Class;

   overriding
   function Is_Conditional (This : Case_Node) return Boolean is (True);

   function Image_Case (Cases : Cases_Array;
                        I     : Enum) return String is
     (I'Img & " => " & Cases (I).Image_One_Line
      & (if I /= Cases'Last
         then ", " & Image_Case (Cases, Enum'Succ (I))
         else ""));

   overriding
   function Image (This : Case_Node) return String is
     ("(case " & Enum_Cases.Name & " is "
      & Image_Case (This.Cases, This.Cases'First) & ")");

   overriding
   function Leaf_Count (This : Case_Node) return Positive;

   overriding
   procedure Print (This    : Case_Node;
                    Prefix  : String;
                    Verbose : Boolean;
                    Sorted  : Boolean)
   is
      use GNAT.IO;
      Tab : constant String := "   ";
   begin
      Put_Line (Prefix & "case " & Enum_Cases.Name & " is");
      for I in This.Cases'Range loop
         if not This.Cases (I).Is_Empty then
            Put_Line (Prefix & Tab & "when "
                      & Utils.To_Mixed_Case (I'Img) & " => "
                      & (if not Verbose
                        then This.Cases (I).Image_One_Line
                        else ""));
            if Verbose then
               Print (This.Cases (I), Prefix & Tab & Tab, Verbose, Sorted);
            end if;
         end if;
      end loop;
   end Print;

   overriding
   function Evaluate (This    : Case_Node;
                      Against : Properties.Vector)
                      return Tree'Class is
   begin
      return Eval : Tree := Empty do
         for I in This.Cases'Range loop
            if Enum_Cases.Is_Satisfied (I, Against) then
               Eval := Eval and This.Cases (I).Evaluate (Against);
            end if;
         end loop;
      end return;
   end Evaluate;

   overriding
   function Leaf_Count (This : Case_Node) return Positive is
      Count : Natural := 0;
   begin
      for Tree of This.Cases loop
         Count := Count + Tree.Leaf_Count;
      end loop;

      return Count;
   end Leaf_Count;

   overriding
   function Flatten (This : Case_Node) return Node'Class is
      Flat : Tree;
   begin
      for T of This.Cases loop
         if not T.Is_Empty then
            Flat := Flat and To_Tree (T.Root.Flatten);
         end if;
      end loop;

      return Flat.Root;
   end Flatten;

   overriding
   procedure To_TOML (This : Case_Node; Parent : TOML.TOML_Value) is
   begin
      raise Unimplemented;
   end To_TOML;

   overriding
   function To_YAML (This : Case_Node) return String is
   begin
      raise Unimplemented;
      return "";
   end To_YAML;

   --------------
   -- New_Case --
   --------------

   function New_Case (Cases : Cases_Array) return Tree is
      (To_Tree (Case_Node'(Cases => Cases)));

end Alire.Conditional_Trees.Cases;
