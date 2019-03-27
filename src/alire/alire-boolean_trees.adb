with Ada.Containers; use Ada.Containers;

with GNAT.IO;

package body Alire.Boolean_Trees is

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
      if L.Is_Empty and then R.Is_Empty then
         return Empty_Tree;
      elsif L.Is_Empty then
         return R;
      elsif R.Is_Empty then
         return L;
      else
         return Merge_Under (Node'(Kind => And_Node), L, R);
      end if;
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (L, R : Tree) return Tree is
   begin
      if L.Is_Empty and then R.Is_Empty then
         return Empty_Tree;
      elsif L.Is_Empty then
         return R;
      elsif R.Is_Empty then
         return L;
      else
         return Merge_Under (Node'(Kind => Or_Node), L, R);
      end if;
   end "or";

   -----------
   -- "not" --
   -----------

   function "not" (T : Tree) return Tree is
   begin
      return Merge_Under (Node'(Kind => Not_Node), T);
   end "not";

   -----------
   -- Check --
   -----------

   function Check (T : Tree; V : Value; If_Empty : Boolean := True) return Boolean is

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
      if T.Is_Empty then
         return If_Empty;
      else
         return Check (Trees.First_Child (T.Root));
      end if;
   end Check;

   ---------------------
   -- Image_Recursive --
   ---------------------

   function Image_Recursive (C : Trees.Cursor; Skeleton : Boolean) return String is
      N : constant Node := Trees.Element (C);
   begin
      case N.Kind is
         when Leaf =>
            if Skeleton then
               return "Leaf";
            else
               return Image (N.Condition.Constant_Reference);
            end if;
         when And_Node =>
            return "(" & Image_Recursive (Trees.First_Child (C), Skeleton) & " and " &
                         Image_Recursive (Trees.Last_Child (C), Skeleton) & ")";
         when Or_Node =>
            return "(" & Image_Recursive (Trees.First_Child (C), Skeleton) & " or " &
                         Image_Recursive (Trees.Last_Child (C), Skeleton) & ")";
         when Not_Node =>
            return "(not " & Image_Recursive (Trees.First_Child (C), Skeleton) & ")";
      end case;
   end Image_Recursive;

   -----------
   -- Image --
   -----------

   function Image (T : Tree) return String is
   begin
      if T.Is_Empty then
         return "(empty tree)";
      else
         return Image_Recursive (Trees.First_Child (T.Root), Skeleton => False);
      end if;
   end Image;

   -----------
   -- Print --
   -----------

   procedure Print (T : Tree) is
   begin
      GNAT.IO.Put_Line (T.Image);
   end Print;

   --------------------
   -- Image_Skeleton --
   --------------------

   function Image_Skeleton (T : Tree) return String is
   begin
      if T.Is_Empty then
         return "(empty tree)";
      else
         return Image_Recursive (Trees.First_Child (T.Root), Skeleton => True);
      end if;
   end Image_Skeleton;

   --------------------
   -- Print_Skeleton --
   --------------------

   procedure Print_Skeleton (T : Tree) is
   begin
      GNAT.IO.Put_Line (T.Image_Skeleton);
   end Print_Skeleton;

end Alire.Boolean_Trees;
