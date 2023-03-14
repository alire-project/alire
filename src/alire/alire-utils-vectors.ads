with Ada.Containers.Indefinite_Vectors;

package Alire.Vectors.Utils.Vectors
is
   generic
      type Index_Type   is positive;
      type Element_type is private;

      with function "="
        (A, B : Element_Type)
         return Boolean;

      with package Containers
        is Ada.Containers.Indefinite_Vectors
          (Index_Type   =  Index_Type,
           Element_Type =  Element_Type);

   package Indefinite
   is
      function Contains
        (Container : Containers.Vector;
         Value     : Element_Type)
         return Boolean;

   end Indefinite;

end Alire.Vectors.Utils.Vectors;
