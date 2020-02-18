package body Alire.Requisites is

   ----------------
   -- Default_To --
   ----------------

   function Default_To (This : Tree; Default : Tree) return Tree is
     (if This.Is_Empty
      then Default
      else This);

   ---------------
   -- Satisfies --
   ---------------

   function Satisfies (R : Requisite'Class;
                       P : Properties.Vector)
                       return Boolean
   is
   begin
      for Prop of P loop
         if R.Satisfies (Prop) then
            return True;
         end if;
      end loop;

      --  Attempt the alternative, more exhaustive form:
      for Prop of P loop
         if R.Satisfies (Prop, P) then
            return True;
         end if;
      end loop;

      return False;
   end Satisfies;

end Alire.Requisites;
