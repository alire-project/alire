private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Multiway_Trees;

generic
   type Value     (<>) is private;
   type Condition (<>) is private;
   with function Check (C : Condition; V : Value) return Boolean;
package Condtrees with Preelaborate is

   --  A package to represent trees of logical expressions

   type Tree is private;

   Empty_Tree : constant Tree;

   --  Tree building

   function Leaf (C : Condition) return Tree;
   function "+"  (C : Condition) return Tree renames Leaf;

   function "and" (L, R : Tree) return Tree;
   function "or"  (L, R : Tree) return Tree;
   function "not" (T    : Tree) return Tree;

   function "and" (L : Tree; R : Condition) return Tree is (L and Leaf (R));
   function "or"  (L : Tree; R : Condition) return Tree is (L or  Leaf (R));
   function "not" (C : Condition) return Tree is (not Leaf (C));

   --  Tree evaluation

   function Check (T : Tree; V : Value) return Boolean;

private

   type Node_Kinds is (Leaf, And_Node, Or_Node, Not_Node);

   package Values is new Ada.Containers.Indefinite_Holders (Value);
   package Conditions is new Ada.Containers.Indefinite_Holders (Condition);

   type Node (Kind : Node_Kinds) is record
      case Kind is
         when Leaf =>
            Condition : Conditions.Holder;
         when others =>
            null;
      end case;
   end record;

   package Trees is new Ada.Containers.Indefinite_Multiway_Trees (Node);

   type Tree is new Trees.Tree with null record;

   Empty_Tree : constant Tree := (Trees.Empty_Tree with null record);

end Condtrees;
