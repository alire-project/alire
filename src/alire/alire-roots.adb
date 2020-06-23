with Ada.Directories;

with Alire.Directories;
with Alire.Environment;
with Alire.Root;
with Alire.TOML_Index;

with GNAT.OS_Lib;

package body Alire.Roots is

   -------------------
   -- Build_Context --
   -------------------

   function Build_Context (This : Root) return Alire.Environment.Context is
   begin
      return Context : Alire.Environment.Context do
         Context.Load (This);
      end return;
   end Build_Context;

   -----------------
   -- Detect_Root --
   -----------------

   function Detect_Root (Path : Any_Path) return Root is
      use GNAT.OS_Lib;
      Alire_Path : constant Any_Path :=
                     Path / Alire.Paths.Working_Folder_Inside_Root;
   begin
      if not Is_Directory (Alire_Path) then
         Trace.Debug ("No alire folder while detecting root at " & Path);
         return New_Invalid_Root.With_Reason ("No alire metadata directory");
      end if;

      declare
         Crate_File : constant String := Directories.Find_Single_File
           (Path      => Alire_Path,
            Extension => ".toml");
      begin
         if Crate_File /= "" then
            declare
               Release : constant Releases.Release :=
                          TOML_Index.Load_Release_From_File (Crate_File);
            begin
               --  Crate loaded properly, we can return a valid root here
               Trace.Debug ("Valid root found at " & Path);
               return New_Root (R    => Release,
                                Path => Ada.Directories.Full_Name (Path),
                                Env  => Alire.Root.Platform_Properties);
            end;
         else
            Trace.Debug ("No crate file found at " & Alire_Path);
            return New_Invalid_Root.With_Reason ("no crate file found");
         end if;
      exception
         when E : others =>
            Trace.Debug ("Crate detection failed while loading toml file:");
            Log_Exception (E);
            return New_Invalid_Root.With_Reason
              ("toml file found but not loadable: " & Crate_File);
      end;
   end Detect_Root;

   ------------------------------
   -- Export_Build_Environment --
   ------------------------------

   procedure Export_Build_Environment (This : Root) is
      Context : Alire.Environment.Context;
   begin
      Context.Load (This);
      Context.Export;
   end Export_Build_Environment;

   -------------------
   -- Project_Paths --
   -------------------

   function Project_Paths (This : Root) return Utils.String_Set
   is
      Paths : Utils.String_Set;
   begin

      --  Add root path from every release in the solution

      for Rel of This.Solution.Releases.Including (Release (This)) loop
         if Rel.Name = Release (This).Name then
            null; -- The root project doesn't require its own path
         else
            Paths.Include (This.Release_Base (Rel.Name));
         end if;

         --  Add extra project paths

         for Path of Rel.Project_Paths (This.Environment) loop
            Paths.Include (This.Release_Base (Rel.Name) / Path);
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

   -----------------------
   -- GPR_Project_Files --
   -----------------------

   function GPR_Project_Files (This         : Root;
                               Exclude_Root : Boolean)
                               return Utils.String_Set
   is
      Files : Utils.String_Set;
   begin

      --  Add files from every release in the solution

      for Rel of This.Solution.Releases.Including (Release (This)) loop

         if (not Exclude_Root or else Rel.Name /= Release (This).Name)
           and then
            Rel.Auto_GPR_With
         then
            for File of Rel.Project_Files
              (This.Environment, With_Path => False)
            loop
               Files.Include (File);
            end loop;
         end if;
      end loop;
      return Files;
   end GPR_Project_Files;

end Alire.Roots;
