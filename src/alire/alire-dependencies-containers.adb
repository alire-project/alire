package body Alire.Dependencies.Containers is

   --------------------
   -- Image_One_Line --
   --------------------

   function Image_One_Line (This : Set) return String is
      Result : AAA.Strings.Vector;
   begin
      if This.Is_Empty then
         return "(empty)";
      end if;

      for Dep of This loop
         Result.Append (Dep.TTY_Image);
      end loop;

      return Result.Flatten (", ");
   end Image_One_Line;

   -----------
   -- Merge --
   -----------

   procedure Merge (This : in out Map;
                    Dep  :        Dependencies.Dependency)
   is
      use type Semantic_Versioning.Extended.Version_Set;
   begin
      if This.Contains (Dep.Crate) then
         declare
            Old : constant Dependencies.Dependency := This (Dep.Crate);
         begin
            if Old /= Dep then
               --  Include should work to replace the dependency, but I'm
               --  getting a tampering error using it (?)
               This.Delete (Dep.Crate);
               This.Insert (Dep.Crate,
                            Dependencies.New_Dependency
                              (Dep.Crate,
                               Old.Versions and Dep.Versions));
            end if;
         end;
      else
         This.Insert (Dep.Crate, Dep);
      end if;
   end Merge;

   ------------
   -- To_Set --
   ------------

   function To_Set (This : List'Class) return Set is
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
