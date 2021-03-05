with Alire.Pseudotypes.Maps;

private generic
package Alire.Conditional_Trees.Case_Nodes is

   --  Non-generic (in the sense of the types used to index the cases)
   --  replacement of Conditional_Tree.Cases

   --  This package provides the case-holding nodes in a conditional tree.
   --  Since clients retrieve such nodes as trees (New_Case below), the whole
   --  new Node class can be hidden in the body.

   package Case_Maps is new Pseudotypes.Maps (Tree);
   --  Every case value points to a subtree

   subtype Map is Case_Maps.Map;

   function New_Case (Unused_Cases : Map) return Tree is (raise Unimplemented);
   --  This function is needed by the case(xx) expression loaders. From a Map
   --  built from the TOML index a Tree is obtained that in reality is a case
   --  node.

end Alire.Conditional_Trees.Case_Nodes;
