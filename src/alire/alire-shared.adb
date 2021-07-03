with Ada.Directories;

with Alire.Config.Edit;
with Alire.Directories;
with Alire.Manifest;
with Alire.Origins;
with Alire.Paths;
with Alire.Root;
with Alire.TTY;
with Alire.Warnings;

package body Alire.Shared is

   use Directories.Operators;

   ---------------
   -- Available --
   ---------------

   function Available return Containers.Release_Set is

      Result : Containers.Release_Set;

      ------------
      -- Detect --
      ------------

      procedure Detect (Item : Ada.Directories.Directory_Entry_Type;
                        Stop : in out Boolean)
      is
         use Ada.Directories;
      begin
         Stop := False;
         if Kind (Item) = Directory then
            if Exists (Full_Name (Item) / Paths.Crate_File_Name) then
               Trace.Debug ("Detected shared release at "
                            & TTY.URL (Full_Name (Item)));

               Result.Include
                 (Releases.From_Manifest
                    (File_Name => Full_Name (Item) / Paths.Crate_File_Name,
                     Source    => Manifest.Local,
                     Strict    => True));
            else
               Warnings.Warn_Once ("Unexpected folder in shared crates path: "
                                   & TTY.URL (Full_Name (Item)));
            end if;

         else
            Warnings.Warn_Once ("Unexpected file in shared crates path: "
                                & TTY.URL (Full_Name (Item)));
         end if;
      end Detect;

   begin
      if Ada.Directories.Exists (Install_Path) then
         Directories.Traverse_Tree
           (Start => Install_Path,
            Doing => Detect'Access);
      end if;

      return Result;
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
      Already_Installed : Boolean := False;
   begin

      --  See if it is a valid installable origin

      if Release.Origin.Kind in Origins.External_Kinds then
         Raise_Checked_Error
           ("Only regular releases can be installed and the requested release"
            & " has origin of kind " & Release.Origin.Kind'Image);
      end if;

      --  See if it can be skipped

      if Available.Contains (Release) then
         Trace.Info ("Skipping installation of already available release: "
                      & Release.Milestone.TTY_Image);
         return;
      end if;

      --  Deploy at the install location

      Release.Deploy (Env             => Root.Platform_Properties,
                      Parent_Folder   => Install_Path,
                      Was_There       => Already_Installed,
                      Perform_Actions => True,
                      Create_Manifest => True);

      if Already_Installed then
         Trace.Warning
           ("Reused previous installation for existing release: "
            & Release.Milestone.TTY_Image);
      end if;

      Put_Info (Release.Milestone.TTY_Image & " installed successfully.");
   end Share;

end Alire.Shared;
