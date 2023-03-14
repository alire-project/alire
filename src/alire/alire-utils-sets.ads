with Ada.Containers.Indefinite_Ordered_Sets;

package Alire.Utils.Sets
is
   generic
      type Element_type is private;
      type Item_Type    is private;

      with function "=" (
         Item    : Item_type;
         Element : Element_Type)
         return Boolean;

      with package Containers
        is new Ada.Containers.Indefinite_Ordered_Sets
          (Element_Type => Element_Type);

   package Indefinite_Ordered
   is
      function Contains
        (Container : Containers.Vector;
         Item      : Item_Type)
         return Boolean;

   end Indefinite_Ordered;

end Alire.Utils.Sets;
