package body Alire.Utils.Sets
is
   package body Ordered
   is
      package body Indefinite
      is
         function Contains
            (
            Item : Item_Type
            )
         return Boolean
         is
            for Element of Container loop
               if Element = Item then
                  return True;
               end if;
            end loop;
            return False;

         end Contains;

      end Indefinite;

   end Ordered;

end Alire.Utils.Sets;
