with Alire.Errors;

with Semantic_Versioning.Basic;
with Semantic_Versioning.Extended;

package body Alire.Containers is

   --------------------------
   -- Contains_Or_Provides --
   --------------------------

   function Contains_Or_Provides (This  : Release_Map;
                                  Crate : Crate_Name) return Boolean
   is (This.Contains (Crate)
       or else
         (for some Rel of This => Rel.Provides (Crate)));

   -----------------------
   -- Element_Providing --
   -----------------------

   function Element_Providing (This  : Release_Map;
                               Crate : Crate_Name)
                               return Releases.Release
   is
   begin
      if This.Contains (Crate) then
         return This (Crate);
      else
         for Rel of This loop
            if Rel.Provides (Crate) then
               return Rel;
            end if;
         end loop;
      end if;

      raise Constraint_Error with Errors.Set
        ("Requested crate not in map: " & Crate.As_String);
   end Element_Providing;

   ---------------
   -- Enumerate --
   ---------------

   function Enumerate (These : Conditional.Dependencies) return Dependency_Map
   is

      procedure Append (C     : in out Dependency_Map;
                        V     : Dependencies.Dependency;
                        Count : Ada.Containers.Count_Type := 1)
      is
         pragma Unreferenced (Count);
      begin
         C.Include (V.Crate, V);
      end Append;

      function Internal is new Conditional.For_Dependencies.Enumerate
        (Collection => Dependency_Map,
         Append     => Append);

   begin
      return Internal (These);
   end Enumerate;

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

   -----------
   -- Merge --
   -----------

   procedure Merge (This : in out Dependency_Map;
                    Dep  :        Dependencies.Dependency)
   is
      use type Dependencies.Dependency;
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
                        Dep  : Dependencies.Dependency)
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
                      Props : Properties.Vector) return Release_Map is
   begin
      return Result : Release_Map do
         for Release of Map loop
            Result.Insert (Release.Name, Release.Whenever (Props));
         end loop;
      end return;
   end Whenever;

end Alire.Containers;
