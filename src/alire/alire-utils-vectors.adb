package body Alire.Vectors.Utils.Vectors
is
   package body Indefinite
   is
      function Contains
        (Container : Containers.Vector;
         Value     : Element_Type)
         return Boolean
      is
         for Element of Container loop
            if Element = Value then
               return True;
            end if;
         end loop;
         return False;
      end Contains;

   end Indefinite;

end Alire.Vectors.Utils.Vectors;
