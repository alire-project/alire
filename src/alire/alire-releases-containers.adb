with AAA.Strings;

with Alire.Errors;

with Semantic_Versioning.Extended;
with Semantic_Versioning.Basic;

package body Alire.Releases.Containers is

   --------------------------
   -- Contains_Or_Provides --
   --------------------------

   function Contains_Or_Provides (This  : Release_Map;
                                  Crate : Crate_Name) return Boolean
   is (This.Contains (Crate)
       or else
         (for some Rel of This => Rel.Provides (Crate)));

   ------------------------
   -- Elements_Providing --
   ------------------------

   function Elements_Providing (This  : Release_Map'Class;
                                Crate : Crate_Name)
                                return Release_Set
   is
      Result : Release_Set;
   begin
      for Rel of This loop
         if Rel.Provides (Crate) then
            Result.Include (Rel);
         end if;
      end loop;

      return Result;
   end Elements_Providing;

   --------------
   -- From_Set --
   --------------

   function From_Set (This : Release_Set)
                      return Release_Set_By_Version.Set
   is
   begin
      return Result : Release_Set_By_Version.Set do
         for Rel of This loop
            Result.Insert (Rel);
         end loop;
      end return;
   end From_Set;

   ------------
   -- Insert --
   ------------

   procedure Insert (Dst : in out Release_Map; Src : Releases.Release) is
   begin
      Dst.Insert (Src.Name, Src);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert (Dst : in out Release_Map; Src : Release_Map) is
   begin
      for E of Src loop
         Dst.Insert (E);
      end loop;
   end Insert;

   ---------------
   -- Inserting --
   ---------------

   function Inserting (Dst : Release_Map;
                       Src : Release_Map)
                       return Release_Map
   is
   begin
      return Result : Release_Map := Dst do
         for E of Src loop
            Result.Insert (E.Name, E);
         end loop;
      end return;
   end Inserting;

   ---------------
   -- Inserting --
   ---------------

   function Inserting (Dst : Release_Map;
                       Src : Releases.Release)
                       return Release_Map
   is (Dst.Inserting (To_Map (Src)));

   ---------------
   -- Excluding --
   ---------------

   function Excluding (Map  : Release_Map;
                       Name : Crate_Name)
                       return Release_Map
   is
   begin
      return Filtered : Release_Map := Map do
         Filtered.Exclude (Name);
      end return;
   end Excluding;

   --------------------
   -- Image_One_Line --
   --------------------

   function Image_One_Line (This : Release_Set) return String is
      Result : UString;
      use UStrings;
   begin
      for Rel of This loop
         if Result /= "" then
            Append (Result, ", ");
         end if;

         Append (Result, Rel.Milestone.TTY_Image);
      end loop;

      return +Result;
   end Image_One_Line;

   ---------------
   -- Including --
   ---------------

   function Including (Map     : Release_Map;
                       Release : Releases.Release)
                       return Release_Map
   is
   begin
      return New_Map : Release_Map := Map do
         New_Map.Include (Release.Name, Release);
      end return;
   end Including;

   --------------
   -- Is_Older --
   --------------

   function Is_Older (This, Than : Releases.Release) return Boolean
   is (This.Version < Than.Version
       or else
         (This.Version = Than.Version
          and then This.Version.Build < Than.Version.Build)
       or else
         (This.Version = Than.Version
          and then This.Version.Build = Than.Version.Build
          and then This.Provides (GNAT_Crate)
          and then Than.Provides (GNAT_Crate)
          and then not AAA.Strings.Has_Suffix (This.Name.As_String, "_native")
          and then AAA.Strings.Has_Suffix (Than.Name.As_String, "_native"))
       or else
         (This.Version = Than.Version
          and then This.Version.Build = Than.Version.Build
          and then Than.Name < This.Name));

   ------------
   -- Remove --
   ------------

   procedure Remove (This    : in out Release_Map;
                     Release : Releases.Release)
   is
   begin
      if This.Contains (Release.Name) then
         This.Exclude (Release.Name);
         return;
      else
         for Mil of Release.Provides loop
            if This.Contains (Mil.Crate) then
               This.Exclude (Mil.Crate);
               return;
            end if;
         end loop;
      end if;

      raise Constraint_Error with Errors.Set
        ("Release not in map: " & Release.Milestone.TTY_Image);
   end Remove;

   ----------------
   -- Satisfying --
   ----------------

   function Satisfying (This : Release_Set;
                        Dep  : Alire.Dependencies.Dependency)
                        return Release_Set
   is
   begin
      return Result : Release_Set do
         for Release of This loop
            if Release.Satisfies (Dep) then
               Result.Include (Release);
            end if;
         end loop;
      end return;
   end Satisfying;

   ---------------------
   -- To_Dependencies --
   ---------------------

   function To_Dependencies (Map : Release_Map)
                             return Conditional.Dependencies
   is
      package Semver renames Semantic_Versioning;
      use Conditional.For_Dependencies;
   begin
      return Deps : Conditional.Dependencies do
         for I in Map.Iterate loop
            Deps :=
              Deps and
              Conditional.New_Dependency
                (Map (I).Name,
                 Semver.Extended.To_Extended
                   (Semver.Basic.Exactly (Map (I).Version)));
         end loop;
      end return;
   end To_Dependencies;

   ------------
   -- To_Map --
   ------------

   function To_Map (R : Releases.Release) return Release_Map is
   begin
      return M : Release_Map do
         M.Include (R.Name, R);
      end return;
   end To_Map;

   --------------
   -- Whenever --
   --------------

   function Whenever (Map   : Release_Map;
                      Props : Alire.Properties.Vector) return Release_Map is
   begin
      return Result : Release_Map do
         for Release of Map loop
            Result.Insert (Release.Name, Release.Whenever (Props));
         end loop;
      end return;
   end Whenever;

end Alire.Releases.Containers;
