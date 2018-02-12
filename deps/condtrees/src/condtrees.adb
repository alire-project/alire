with Ada.Containers; use Ada.Containers;

with GNAT.IO;

package body Condtrees is

   ----------
   -- Leaf --
   ----------

   function Leaf (C : Condition) return Tree is
   begin
      return T : Tree do
         T.Append_Child (T.Root, Node'(Leaf, Conditions.To_Holder (C)));
      end return;
   end Leaf;

   -----------------
   -- Merge_Under --
   -----------------

   function Merge_Under (N : Node; L, R : Tree := Empty_Tree) return Tree is
      use Trees;
   begin
      return T : Tree do
         T.Append_Child (Parent => T.Root, New_Item => N);

         declare
            Op : constant Cursor := First_Child (T.Root);
         begin
            pragma Assert (Element (Op) = N);

            if L /= Empty_Tree then
               T.Copy_Subtree (Parent => Op,
                               Before => No_Element,
                               Source => First_Child (L.Root));
            end if;

            if R /= Empty_Tree then
               T.Copy_Subtree (Parent => Op,
                               Before => No_Element,
                               Source => First_Child (R.Root));
            end if;
         end;
      end return;
   end Merge_Under;

   -----------
   -- "and" --
   -----------

   function "and" (L, R : Tree) return Tree is
   begin
      return Merge_Under (Node'(Kind => And_Node), L, R);
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (L, R : Tree) return Tree is
   begin
      return Merge_Under (Node'(Kind => Or_Node), L, R);
   end "or";

   -----------
   -- "not" --
   -----------

   function "not" (T : Tree) return Tree is
      use Trees;
   begin
      return Merge_Under (Node'(Kind => Not_Node), T);
   end "not";

   -----------
   -- Check --
   -----------

   function Check (T : Tree; V : Value) return Boolean is

      function Check (C : Trees.Cursor) return Boolean is
         N : constant Node := Trees.Element (C);
      begin
         case N.Kind is
            when Leaf =>
               return Check (N.Condition.Element, V);
            when And_Node =>
               return Check (Trees.First_Child (C)) and then Check (Trees.Last_Child (C));
            when Or_Node =>
               return Check (Trees.First_Child (C)) or else Check (Trees.Last_Child (C));
            when Not_Node =>
               return not Check (Trees.First_Child (C));
         end case;
      end Check;

   begin
      return Check (Trees.First_Child (T.Root));
   end Check;

   --------------------
   -- Print_Skeleton --
   --------------------

   procedure Print_Skeleton (T : Tree) is
      use GNAT.IO;
      use Trees;

      function Image (C : Trees.Cursor) return String is
         N : constant Node := Trees.Element (C);
      begin
         case N.Kind is
            when Leaf =>
               return "Leaf";
            when And_Node =>
               return "(" & Image (Trees.First_Child (C)) & " and " & Image (Trees.Last_Child (C)) & ")";
            when Or_Node =>
               return "(" & Image (Trees.First_Child (C)) & " or " & Image (Trees.Last_Child (C)) & ")";
            when Not_Node =>
               return "(not " & Image (Trees.First_Child (C)) & ")";
         end case;
      end Image;

   begin
      if T.Is_Empty then
         Put_Line ("(null tree)");
      else
         Put_Line (Image (Trees.First_Child (T.Root)));
      end if;
   end Print_Skeleton;

end Condtrees;
