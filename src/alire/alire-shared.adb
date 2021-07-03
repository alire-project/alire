with Alire.Config.Edit;
with Alire.Directories;
with Alire.Lockfiles;
with Alire.Paths;
with Alire.Root;

with GNAT.OS_Lib;

with Semantic_Versioning;

package body Alire.Shared is

   use Directories.Operators;

   Lockfile : constant Relative_Path :=
                Paths.Cache_Folder_Inside_Working_Folder
                  / "alire.lock";

   ---------------
   -- Available --
   ---------------

   function Available return Solutions.Solution is
   begin
      if GNAT.OS_Lib.Is_Regular_File (Config.Edit.Path / Lockfile) then
         begin
            return Lockfiles.Read (Config.Edit.Path / Lockfile).Solution;
         exception
            when E : others =>
               Log_Exception (E);
               Put_Warning ("Installed releases need to be reinstalled.");
               Directories.Force_Delete (Config.Edit.Path / Lockfile);
               return Solutions.Empty_Valid_Solution;
               --  TODO: we can rebuild this file by loading all crate
               --  manifests found in the immediate subfolders. Indeed, this
               --  is better than relying on a lockfile, as everything will
               --  be always synced.
         end;
      else
         return Solutions.Empty_Valid_Solution;
      end if;
   end Available;

   ------------------
   -- Install_Path --
   ------------------

   function Install_Path return String
   is (Config.Edit.Path
       / Paths.Cache_Folder_Inside_Working_Folder
       / Paths.Deps_Folder_Inside_Cache_Folder);

   -----------
   -- Share --
   -----------

   procedure Share (Release : Releases.Release)
   is
      use type Semantic_Versioning.Version;
      Current : Solutions.Solution := Available;
      Already_Installed : Boolean := False;
   begin

      --  See if it can be skipped

      if Current.Depends_On (Release.Name)
        and then Current.State (Release.Name).Release.Version = Release.Version
      then
         Trace.Info ("Skipping installation of already available release: "
                      & Release.Milestone.TTY_Image);
         return;
      end if;

      --  Or if it conflicts with another install. TODO: consider provides here
      --  in the future.

      if Current.Depends_On (Release.Name) then
         Raise_Checked_Error
           ("A different version is already installed: "
            & Current.State (Release.Name).Release.Milestone.TTY_Image
            & "; please remove it before installing another version");
      end if;

      --  Deploy at the install location

      Release.Deploy (Env             => Root.Platform_Properties,
                      Parent_Folder   => Install_Path,
                      Was_There       => Already_Installed,
                      Perform_Actions => True);

      if Already_Installed then
         Trace.Warning
           ("Registering shared installation for existing release: "
            & Release.Milestone.TTY_Image);
      end if;

      --  Store the new release in the shared config lockfile

      Current := Current.Including (Release        => Release,
                                    Env            => Root.Platform_Properties,
                                    Add_Dependency => True);

      Lockfiles.Write (Contents => (Solution => Current),
                       Filename => Config.Edit.Path / Lockfile);

      Put_Info (Release.Milestone.TTY_Image & " installed successfully.");
   end Share;

end Alire.Shared;
