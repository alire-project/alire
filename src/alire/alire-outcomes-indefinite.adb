package body Alire.Outcomes.Indefinite is

   ---------------
   -- To_Holder --
   ---------------

   function To_Holder (R : Result) return Definites.List is
   begin
      return L : Definites.List do
         L.Append (R);
      end return;
   end To_Holder;

end Alire.Outcomes.Indefinite;
