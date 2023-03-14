with Ada.Containers.Indefinite_Vectors;

package Alire.Utils.Vectors
is
   generic
      type Index_Type   is range <>;
      type Element_type is private;

      with function "="
        (Left, Right : Element_Type)
         return Boolean
      is <>;

      with package Containers
        is new Ada.Containers.Indefinite_Vectors
          (Index_Type   => Index_Type,
           Element_Type => Element_Type);

   package Indefinite
   is
      function Contains
        (Container : Containers.Vector;
         Item      : Element_Type)
         return Boolean;

   end Indefinite;

end Alire.Utils.Vectors;
