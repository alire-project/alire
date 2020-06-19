package body Alire.Roots is

   -------------------
   -- Project_Paths --
   -------------------

   function Project_Paths (This : Root) return Utils.String_Set
   is
      Paths : Utils.String_Set;
      Base  : constant Any_Path := Path (This);
   begin

      --  Add root path from every release in the solution

      for Rel of This.Solution.Releases.Including (Release (This)) loop
         if Rel.Name = Release (This).Name then
            null; -- The root project doesn't require its own path
         else
            Paths.Include (Base
                           / Alire.Paths.Working_Folder_Inside_Root
                           / Alire.Paths.Dependency_Dir_Inside_Working_Folder
                           / Rel.Unique_Folder);
         end if;

         --  Add extra project paths

         for Path of Rel.Project_Paths (This.Environment) loop
            if Rel.Name = Release (This).Name then
               Paths.Include (Base / Path);
            else
               Paths.Include
                 (Base
                  / Alire.Paths.Working_Folder_Inside_Root
                  / Alire.Paths.Dependency_Dir_Inside_Working_Folder
                  / Rel.Unique_Folder
                  / Path);
            end if;
         end loop;
      end loop;

      --  Add paths for pinned folders

      for Link of This.Solution.Links loop
         for Path of This.Solution.State (Link.Crate).Link.Project_Paths loop
            Paths.Include (Path); -- These are absolute
         end loop;
      end loop;

      return Paths;
   end Project_Paths;

end Alire.Roots;
