package body Alire.Dependencies.Containers is

   function To_Set (This : List) return Sets.Set is
   begin
      return Result : Set do
         for Dep of This loop
            Result.Include (Dep);
            --  We include instead of inserting because enumeration of case
            --  expressions may give the same dependency more than once.
         end loop;
      end return;
   end To_Set;

end Alire.Dependencies.Containers;
