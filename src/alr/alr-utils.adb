package body Alr.Utils is

   --------------
   -- Contains --
   --------------

   function Contains (V : AAA.Strings.Vector; Subst : String) return Boolean is
   begin
      for Str of V loop
         if AAA.Strings.Contains (Str, Subst) then
            return True;
         end if;
      end loop;

      return False;
   end Contains;

end Alr.Utils;
