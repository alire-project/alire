package body Alire.Vectors.Utils.Vectors
is
   package body Indefinite
   is
      function Contains
        (Container : Containers.Vector;
         Item      : Element_Type)
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

end Alire.Vectors.Utils.Vectors;
