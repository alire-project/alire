with Ada.Calendar;
with Ada.Directories;

with Alire.Directories;
with Alire.Environment;
with Alire.Lockfiles;
with Alire.OS_Lib;
with Alire.Paths;
with Alire.Root;
with Alire.TOML_Index;
with Alire.Workspace;

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
      use Alire.OS_Lib;
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

   -------------------
   -- Project_Paths --
   -------------------

   function Project_Paths (This : Root) return Utils.String_Set
   is
      use Alire.OS_Lib;
      Paths : Utils.String_Set;
   begin

      for Rel of This.Solution.Releases.Including (Release (This)) loop
         --  Add project paths from each release

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

      --  To match the output of root crate paths and Ada.Directories full path
      --  normalization, a path separator in the last position is removed.
      return Result : Utils.String_Set do
         for Path of Paths loop
            if Path'Length /= 0
              and then

              --  The paths provided by crates manifests are expected to use
              --  UNIX directory separator. So we need to handle both UNIX and
              --  OS separators.
              Path (Path'Last) in '/' | GNAT.OS_Lib.Directory_Separator
            then
               Result.Include (Path (Path'First .. Path'Last - 1));
            else
               Result.Include (Path);
            end if;
         end loop;
      end return;
   end Project_Paths;

   --------------
   -- Solution --
   --------------

   function Solution (This : Root) return Solutions.Solution is
   begin
      --  TODO: This probably is a good target for caching unless file
      --  timestamp has changed.
      return Lockfiles.Read (This.Lock_File);
   end Solution;

   -----------------
   -- Environment --
   -----------------

   function Environment (This : Root) return Properties.Vector
   is (This.Environment);

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : Root) return Boolean is (This.Valid);

   ----------------------
   -- New_Invalid_Root --
   ----------------------

   function New_Invalid_Root return Root is
     (Valid => False, Reason => +"");

   -----------------
   -- With_Reason --
   -----------------

   function With_Reason (This : Root; Reason : String) return Root is
     (Valid  => False,
      Reason => +Reason);

   --------------------
   -- Invalid_Reason --
   --------------------

   function Invalid_Reason (This : Root) return String is
     (+This.Reason);

   --------------
   -- New_Root --
   --------------

   function New_Root (Name : Crate_Name;
                      Path : Absolute_Path;
                      Env  : Properties.Vector) return Root is
     (True,
      Env,
      +Path,
      Containers.To_Release_H (Releases.New_Working_Release (Name)));

   --------------
   -- New_Root --
   --------------

   function New_Root (R    : Releases.Release;
                      Path : Absolute_Path;
                      Env  : Properties.Vector) return Root is
     (True,
      Env,
      +Path,
      Containers.To_Release_H (R));

   ----------
   -- Path --
   ----------

   function Path (This : Root) return Absolute_Path is (+This.Path);

   -------------
   -- Release --
   -------------

   function Release (This : Root) return Releases.Release is
     (This.Release.Constant_Reference);

   -------------
   -- Release --
   -------------

   function Release (This  : Root;
                     Crate : Crate_Name) return Releases.Release is
     (if This.Release.Element.Name = Crate
      then This.Release.Element
      else This.Solution.State (Crate).Release);

   use OS_Lib;

   ------------------
   -- Release_Base --
   ------------------

   function Release_Base (This : Root; Crate : Crate_Name) return Any_Path is
     (if This.Release.Element.Name = Crate then
         +This.Path
      elsif This.Solution.State (Crate).Is_Solved then
         This.Dependencies_Dir
         / Release (This, Crate).Unique_Folder
      elsif This.Solution.State (Crate).Is_Linked then
         This.Solution.State (Crate).Link.Path
      else
         raise Program_Error with "release must be either solved or linked");

   ---------------
   -- Lock_File --
   ---------------

   function Lock_File (This : Root) return Absolute_Path is
     (Lockfiles.File_Name
        (This.Release.Constant_Reference.Name,
         +This.Path));

   ----------------
   -- Crate_File --
   ----------------

   function Crate_File (This : Root) return Absolute_Path is
     (This.Working_Folder /
        This.Release.Constant_Reference.Name_Str &
        Paths.Crate_File_Extension_With_Dot);

   ----------------------
   -- Dependencies_Dir --
   ----------------------

   function Dependencies_Dir (This : Root) return Absolute_Path is
      (This.Working_Folder / "cache" / "dependencies");

   --------------------
   -- Working_Folder --
   --------------------

   function Working_Folder (This : Root) return Absolute_Path is
     ((+This.Path) / "alire");

   ----------------------------
   -- Sync_Solution_And_Deps --
   ----------------------------

   procedure Sync_Solution_And_Deps (This : Root) is
      use Ada.Directories;
      use type Ada.Calendar.Time;
   begin
      if Modification_Time (This.Crate_File) >
        Modification_Time (This.Lock_File)
      then
         Trace.Info ("Detected changes in manifest, updating workspace...");
         Workspace.Update_And_Deploy_Dependencies (This);

      elsif (for some Rel of This.Solution.Releases =>
               This.Solution.State (Rel.Name).Is_Solved and then
               not GNAT.OS_Lib.Is_Directory (This.Release_Base (Rel.Name)))
      then
         Trace.Info ("Detected missing dependencies, updating workspace...");
         --  Some dependency is missing; redeploy. Should we clean first ???
         Workspace.Deploy_Dependencies
           (Root     => This,
            Solution => This.Solution,
            Deps_Dir => This.Dependencies_Dir);
      end if;
   end Sync_Solution_And_Deps;

end Alire.Roots;
