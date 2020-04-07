with Alire.Origins;
with Alire.OS_Lib;
with Alire.Paths;

with Semantic_Versioning.Basic;
with Semantic_Versioning.Extended;

package body Alire.Containers is

   ------------
   -- Insert --
   ------------

   procedure Insert (Dst : in out Release_Map; Src : Release_Map) is
   begin
      for E of Src loop
         Dst.Insert (E.Name, E);
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
            if E.Name /= E.Provides then
               Result.Insert (E.Provides, E);
            end if;
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

   -------------------
   -- Project_Paths --
   -------------------

   function Project_Paths (Map   : Release_Map;
                           Base  : Absolute_Path;
                           Root  : Releases.Release;
                           Props : Properties.Vector)
                        return Utils.String_Set
   is
      use all type Origins.Kinds;
      use OS_Lib;

      -------------------
      -- Rel_Base_Path --
      -------------------

      function Rel_Base_Path (Release : Releases.Release) return Relative_Path
      --  Returns the base path that applies to a release, depending on whether
      --  it's a child, it's the root, or it's a regular one.
      is (if Release.Origin.Kind = Child
          then Rel_Base_Path (Map.Element (Release.Origin.Parent))
          elsif Release.Name = Root.Name
          then ".."
          else Paths.Working_Deps_Path / Release.Unique_Folder);

      Set : Utils.String_Set;
   begin
      for Rel of Map.Including (Root) loop

         --  Add root dirs of dependencies

         if Rel.Name /= Root.Name and then Rel.Origin.Kind /= Child then
            Set.Include (Base / Paths.Working_Deps_Path / Rel.Unique_Folder);
         end if;

         --  Add extra project paths, for all releases

         for Path of Rel.Project_Paths (Props) loop
            Set.Include (Base / Rel_Base_Path (Rel) / Path);
         end loop;
      end loop;

      return Set;
   end Project_Paths;

   ---------------------
   -- To_Dependencies --
   ---------------------

   function To_Dependencies (Map : Release_Map)
                             return Conditional.Dependencies
   is
      package Semver renames Semantic_Versioning;
      use Conditional.For_Dependencies;
      use Crate_Release_Maps;
   begin
      return Deps : Conditional.Dependencies do
         for I in Map.Iterate loop
            if Key (I) = Map (I).Provides then -- Avoid duplicates
               Deps :=
                 Deps and
                 Conditional.New_Dependency
                   (Map (I).Name,
                    Semver.Extended.To_Extended
                      (Semver.Basic.Exactly (Map (I).Version)));
            end if;
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

end Alire.Containers;
