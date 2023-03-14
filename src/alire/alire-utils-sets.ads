with Ada.Containers.Indefinite_Ordered_Sets;

package Alire.Utils.Sets
is
   generic
      type Element_type (<>) is private;
 
      with package Sets is new
         Ada.Containers.Indefinite_Ordered_Sets (
            Element_Type => Element_Type,
            "="          => <>,
            "<"          => <>);
   package Ordered
   is
      generic
         type Item_Type (<>)    is private;

         with function "=" (
            Item    : Item_Type;
            Element : Element_Type)
            return Boolean;
   
      package Indefinite
      is
         function Contains
            (
            Container : Sets.Set;
            Item      : Item_Type
            )
         return Boolean;

      end Indefinite;

   end Ordered;

end Alire.Utils.Sets;

