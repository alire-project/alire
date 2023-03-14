with Ada.Containers.Indefinite_Ordered_Sets;

package Alire.Utils.Sets
is
   generic
--      type Element_type is private;
      type Item_Type (<>) is private;

      with package Sets is new
         Ada.Containers.Indefinite_Ordered_Sets (
            Element_Type => <>,
            "="          => <>,
            "<"          => <>);

      with function "=" (
         Item    : Item_Type;
         Element : Sets.Element_Type)
         return Boolean;
   
   package Indefinite_Ordered
   is
      function Contains
        (Container : Sets.Set;
         Item      : Item_Type)
         return Boolean;

   end Indefinite_Ordered;

end Alire.Utils.Sets;
