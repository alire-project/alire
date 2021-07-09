with Semantic_Versioning.Extended;

package body Alire.Provides is

   --------------------
   -- Image_One_Line --
   --------------------

   function Image_One_Line (This : Equivalences) return String is
      use UStrings;
      Result : UString;
      First  : Boolean := True;
   begin
      if This.Is_Empty then
         return "(nothing)"; -- not visible anywhere if all goes well
      end if;

      for Equiv of This loop
         if not First then
            Append (Result, ", ");
            First := False;
         end if;

         Append (Result, Equiv.TTY_Image);
      end loop;

      return +Result;
   end Image_One_Line;

   ---------------
   -- Satisfies --
   ---------------

   function Satisfies (This : Equivalences;
                       Dep  : Dependencies.Dependency)
                       return Boolean
   is (for some Milestone of This =>
         Semantic_Versioning.Extended.Is_In (Milestone.Version, Dep.Versions));

   -------------
   -- To_TOML --
   -------------

   function To_TOML (This : Equivalences) return TOML.TOML_Value is
      use TOML;
      Result : constant TOML_Value := Create_Array;
   begin
      for Equiv of This loop
         Result.Append (Create_String (Equiv.Image));
      end loop;

      return Result;
   end To_TOML;

end Alire.Provides;
