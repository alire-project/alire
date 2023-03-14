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

   package Indefinite_Ordered
   is
      generic
         type Item_Type (<>) is private;
  
         Set : Sets.Set;

         with function "=" (
            Element : Element_Type;
            Item    : Item_Type)
            return Boolean;
   
      function Contains (Item : Item_Type) return Boolean;

   end Indefinite_Ordered;

end Alire.Utils.Sets;

