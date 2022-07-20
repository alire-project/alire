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
         end if;

         Append (Result, Equiv.TTY_Image);
         First := False;
      end loop;

      return +Result;
   end Image_One_Line;

   ---------------
   -- Satisfies --
   ---------------

   function Satisfies (This : Equivalences;
                       Dep  : Dependencies.Dependency'Class)
                       return Boolean
   is (for some Milestone of This =>
          Milestone.Crate = Dep.Crate
       and then
         Semantic_Versioning.Extended.Is_In (Milestone.Version, Dep.Versions));

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue) return Equivalences is
      use TOML;
      TOML_Equivs : constant TOML_Value := From.Unwrap;
   begin
      return Result : Equivalences do
         for I in 1 .. TOML_Equivs.Length loop
            From.Assert (TOML_Equivs.Item (I).Kind = TOML_String,
                         "expected a string describing a milestone, but got: "
                         & TOML_Equivs.Item (I).Kind'Image);

            Result.Append
              (Milestones.New_Milestone (TOML_Equivs.Item (I).As_String));
         end loop;
      end return;
   end From_TOML;

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

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Crate_Provider_Map
   is
   begin
      return Result : Crate_Provider_Map do
         loop
            declare
               Val : TOML.TOML_Value;
               Key : constant String := From.Pop (Val);
               Set : Containers.Crate_Name_Sets.Set;
            begin
               exit when Key = "";
               From.Assert (Val.Kind in TOML.TOML_Array,
                            "expected array of strings but got: "
                            & Val.Kind'Image);
               for I in 1 .. Val.Length loop
                  Set.Insert (+Val.Item (I).As_String);
               end loop;

               Result.Insert (+Key, Set); -- Store Provided --> Providers
            end;
         end loop;
      end return;
   end From_TOML;

   -------------
   -- To_TOML --
   -------------

   function To_TOML (This : Crate_Provider_Map) return TOML.TOML_Value is
      use Crate_Providers_Maps;
   begin
      return Result : constant TOML.TOML_Value := TOML.Create_Table do
         for I in This.Iterate loop
            declare
               Val : constant TOML.TOML_Value := TOML.Create_Array;
               --  A crate name array
            begin
               for Provider of This (I) loop
                  Val.Append (TOML.Create_String (Provider.As_String));
               end loop;

               Result.Set (Key (I).As_String, Val);
            end;
         end loop;
      end return;
   end To_TOML;

end Alire.Provides;
