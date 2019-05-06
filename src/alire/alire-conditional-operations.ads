with Alire.Containers;
with Alire.Releases;

package Alire.Conditional.Operations is

   function Contains (Tree : Dependencies;
                      R : Releases.Release)
                      return Boolean;

   function Contains_Some (Tree : Dependencies;
                           Map  : Containers.Release_Map)
                           return Boolean;
   --  If any in Map is also in Tree

private

   use Conditional.For_Dependencies;

   function Contains (Tree : Dependencies;
                      R    : Releases.Release)
                      return Boolean
   is (for some I in Tree.Iterate =>
          Tree (I).Kind = Value and then R.Satisfies (Tree (I).Value)
      );

   function Contains_Some (Tree : Dependencies;
                           Map  : Containers.Release_Map)
                           return Boolean
   is (for some R of Map => Contains (Tree, R));

end Alire.Conditional.Operations;
