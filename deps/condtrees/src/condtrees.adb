with Ada.Containers;

with GNAT.IO;

package body Condtrees is

   ----------
   -- Leaf --
   ----------

   function Leaf (C : Condition) return Tree is
   begin
      return T : Tree do
         T.Append_Child (T.Root, Node'(Leaf, Conditions.To_Holder (C)));
         pragma Assert (Trees.Has_Element (Trees.First_Child (T.Root)));
      end return;
   end Leaf;

   -----------
   -- "and" --
   -----------

   function "and" (L, R : Tree) return Tree is
   begin
      return T : Tree do
         T.Append_Child (T.Root, Node'(Kind => And_Node));
         pragma Assert (Trees.Has_Element (Trees.First_Child (T.Root)));

         T.Append_Child (Trees.First_Child (T.Root),
                         Trees.First_Child_Element (L.Root));

         T.Append_Child (Trees.First_Child (T.Root),
                         Trees.First_Child_Element (R.Root));
      end return;
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (L, R : Tree) return Tree is
   begin
      return T : Tree do
         T.Append_Child (T.Root, Node'(Kind => Or_Node));
         pragma Assert (Trees.Has_Element (Trees.First_Child (T.Root)));

         T.Append_Child (Trees.First_Child (T.Root),
                         Trees.First_Child_Element (L.Root));

         T.Append_Child (Trees.First_Child (T.Root),
                         Trees.First_Child_Element (R.Root));
      end return;
   end "or";

   -----------
   -- "not" --
   -----------

   function "not" (T : Tree) return Tree is
      use Ada.Containers;
      use Trees;
   begin
      return New_T : Tree do
         New_T.Append_Child (New_T.Root, Node'(Kind => Not_Node));
         pragma Assert (Trees.Has_Element (Trees.First_Child (New_T.Root)));

         New_T.Append_Child (Trees.First_Child (New_T.Root),
                             Trees.First_Child_Element (T.Root));
         pragma Assert (Trees.Child_Count (New_T.Root) = 2);
         pragma Assert (Trees.Depth (New_T.Root) = 2);
      end return;
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
