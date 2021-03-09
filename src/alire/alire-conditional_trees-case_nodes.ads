with Alire.Expressions.Maps;

private generic
package Alire.Conditional_Trees.Case_Nodes with Preelaborate is

   --  NOTE: this package must be instantiated at library level

   --  This package provides the case-holding nodes in a conditional tree.
   --  Since clients retrieve such nodes as trees (New_Case below), the whole
   --  new Node class can be hidden in the body.

   package Case_Maps is new Expressions.Maps (Tree);
   --  Every case value points to a subtree

   subtype Map is Case_Maps.Map;

   function New_Case (Cases : Map) return Tree;
   --  Wrap an expression map as a case node for the conditional tree

end Alire.Conditional_Trees.Case_Nodes;
