package body Alire.Vectors.Utils.Sets
is
   package body Indefinite_Ordered
   is
      function Contains (
         Container : Sets.Set;
         Item      : Item_Type)
         return Boolean
      is
         for Element of Container loop
            if Element = Item then
               return True;
            end if;
         end loop;
         return False;

      end Contains;

   end Indefinite_Ordered;

end Alire.Vectors.Utils.Sets;
