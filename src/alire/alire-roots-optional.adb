package body Alire.Roots.Optional is

   -----------------
   -- Detect_Root --
   -----------------

   function Detect_Root (Path : Any_Path) return Optional.Root is
   begin
      if Path /= "" then
         return New_Result (Roots.Detect_Root (Path));
      else
         return Outcome_Failure ("No candidate folder given");
      end if;
   exception
      when E : others =>
         return Outcome_From_Exception (E);
   end Detect_Root;

end Alire.Roots.Optional;
