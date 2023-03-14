with Ada.Containers.Indefinite_Ordered_Sets;

package Alire.Utils.Sets
is
   generic
      type Element_Type (<>) is private;
 
      with package Sets is new
         Ada.Containers.Indefinite_Ordered_Sets (
            Element_Type => Element_Type,
            "="          => <>,
            "<"          => <>);

   package Ordered
   is
      generic
         type Item_Type (<>) is private;

         with function "=" (
            Set     : Sets.Set;
            Element : Element_Type;
            Item    : Item_Type)
            return Boolean;
   
      package Indefinite
      is
         function Contains
            (
            Item : Item_Type
            )
         return Boolean;

      end Indefinite;

   end Ordered;

end Alire.Utils.Sets;

