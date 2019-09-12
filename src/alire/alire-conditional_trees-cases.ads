with Alire.Requisites.Cases;

generic
   with package Requisite_Cases is new Requisites.Cases (<>);
package Alire.Conditional_Trees.Cases with Preelaborate is

   --  This package provides the case-holding nodes in a conditional tree.
   --  Since clients retrieve such nodes as trees (New_Case below), the whole
   --  new Node class can be hidden in the body.

   subtype Enum is Requisite_Cases.Enum;

   type Cases_Array is array (Enum) of Tree;
   --  Every case points to a dependency tree, that at leaves will have
   --  a single dependency.

   function New_Case (Cases : Cases_Array) return Tree;
   --  This function is needed by the case(xx) expression loaders.

end Alire.Conditional_Trees.Cases;
