package Alire.Utils.Comparisons with Preelaborate is

   type Result is (Left, Equal, Right);

   type Bool_Result is (Left, Right, None, Both);

   -------------
   -- Compare --
   -------------

   generic
      type Comparable (<>) is limited private;
      with function "<" (L, R : Comparable) return Boolean is <>;
   function Compare (L, R : Comparable) return Result
     with Post => Compare'Result =
       (if L < R
          then Left
            else (if R < L
                then Right
                  else Equal));

   ---------------
   -- Which_One --
   ---------------

   function Which_One (L, R : Boolean) return Bool_Result
   is (if L and then not R then
          Left
       elsif R and then not L then
          Right
       elsif L then
          Both
       else
          None);

private

   -------------
   -- Compare --
   -------------

   function Compare (L, R : Comparable) return Result
   is (if L < R then
          Left
       elsif R < L then
          Right
       else
          Equal);

end Alire.Utils.Comparisons;
