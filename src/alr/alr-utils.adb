package body Alr.Utils is

   --------------
   -- Contains --
   --------------

   function Contains (V : String_Vector; Subst : String) return Boolean is
   begin
      for Str of V loop
         if Contains (Str, Subst) then
            return True;
         end if;
      end loop;

      return False;
   end Contains;

end Alr.Utils;
