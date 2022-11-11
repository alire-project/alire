package body Alr.Utils is

   --------------
   -- Contains --
   --------------

   function Contains (V : AAA.Strings.Vector; Subst : String) return Boolean is
   begin
      return (for some Str of V => AAA.Strings.Contains (Str, Subst));
   end Contains;

end Alr.Utils;
