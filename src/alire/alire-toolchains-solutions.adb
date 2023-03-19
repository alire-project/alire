with AAA.Strings;

with Alire.Config.Edit;
with Alire.Directories;
with Alire.Index;
with Alire.Paths;
with Alire.Platforms.Folders;
with Alire.Root;
with Alire.Shared;

with CLIC.User_Input;

package body Alire.Toolchains.Solutions is

   -------------------
   -- Migrate_Cache --
   -------------------

   procedure Migrate_Cache is
      use Directories.Operators;
      Config_Flag : constant String := "alire.cache_migrated";
      Old_Dir     : constant Absolute_Path
        := Platforms.Folders.Config
           / Paths.Cache_Folder_Inside_Working_Folder;
   begin
      if not Config.Edit.Is_At_Default_Dir then
         return; -- Not using default configuration
      end if;

      if Config.DB.Get (Config_Flag, False) then
         return; -- Already migrated
      end if;

      if not Directories.Is_Directory (Old_Dir / "dependencies") then
         Config.Edit.Set_Globally (Config_Flag, "true");
         return; -- Nothing to migrate
      end if;

      Put_Info
        ("The storage location for compilers has changed to better adhere to "
         & "best practices.");
      Trace.Always ("  Old location: " & TTY.URL (Old_Dir));
      Trace.Always ("  New Location: " & TTY.URL (Platforms.Folders.Cache));

      Trace.Always ("");
      Put_Info ("Alire will continue operating normally in any case, "
                & "but you can choose what to do in this one-time operation to"
                & " avoid large redownloads.");
      Put_Info ("Unless you are using the old location explicitly somehow, "
                & "it can be safely deleted.");
      Trace.Always ("");

      declare
         type Choices is (Zero, Copy, Move, Pass);
         Choice : constant Choices := Choices'Val (
           CLIC.User_Input.Query_Multi
             (Question => "Please choose your migration preference:",
              Choices  => AAA.Strings.Empty_Vector
              .Append ("Copy toolchains to new location")
              .Append ("Move toolchains to new location")
              .Append ("Do nothing (compilers will be redownloaded on demand)")
             ));
      begin
         case Choice is
            when Zero => raise Program_Error with "Query_Multi violated Post";
            when Copy =>
               Put_Info ("Copying...");
               Directories.Create_Tree (Platforms.Folders.Cache);
               Directories.Merge_Contents (Src => Old_Dir,
                                           Dst => Platforms.Folders.Cache,
                                           Skip_Top_Level_Files  => False,
                                           Fail_On_Existing_File => False,
                                           Remove_From_Source    => False);
               Put_Success ("Contents copied, the old location can be deleted"
                            & " at your convenience.");
            when Move =>
               Put_Info ("Moving...");
               Directories.Create_Tree (Platforms.Folders.Cache);
               Directories.Merge_Contents (Src => Old_Dir,
                                           Dst => Platforms.Folders.Cache,
                                           Skip_Top_Level_Files  => False,
                                           Fail_On_Existing_File => False,
                                           Remove_From_Source    => True);
               Directories.Force_Delete (Old_Dir);
               Put_Success ("Contents moved successfully.");
            when Pass =>
               Put_Info ("Did nothing, compilers will be redownloaded on "
                         & " demand. The old location can be deleted at your"
                         & " convenience.");
         end case;
      end;

      Config.Edit.Set_Globally (Config_Flag, "true");
   exception
      when E : others =>
         --  Since this involves HD use, a number of things can go wrong...
         --  Don't reattempt, compilers will be redownloaded and the old
         --  location will have to be deleted manually.
         Log_Exception (E, Warning);
         Config.Edit.Set_Globally (Config_Flag, "true");
         Put_Warning ("The operation failed and will not be reattempted. "
                      & " Compilers will be downloaded again when needed. "
                      & " You can delete the old location manually to recover "
                      & "disk space.");
   end Migrate_Cache;

   -------------------
   -- Add_Toolchain --
   -------------------

   function Add_Toolchain (Solution : Alire.Solutions.Solution)
                              return Alire.Solutions.Solution
   is

      ------------------------
      -- Redeploy_If_Needed --
      ------------------------

      procedure Redeploy_If_Needed (Mil : Milestones.Milestone) is
         use type Milestones.Milestone;
      begin
         --  Check that is not already there
         if (for some Rel of Shared.Available => Rel.Milestone = Mil) then
            return;
         end if;

         --  It must be redeployed
         Put_Warning ("Tool " & Mil.TTY_Image & " is missing, redeploying...");

         Shared.Share (Index.Find (Mil.Crate, Mil.Version));
      end Redeploy_If_Needed;

      Result : Alire.Solutions.Solution := Solution;
   begin

      --  For the users' sake, offer to relocate a toolchain in the former
      --  cache location to the new location as gnat downloads are quite heavy.

      Migrate_Cache;

      --  For every tool in the toolchain that does not appear in the solution,
      --  we will insert the user-configured tool, if any.

      for Tool of Toolchains.Tools loop
         if Solution.Depends_On (Tool) then
            Trace.Debug
              ("Toolchain environment: solution already depends on "
               & Solution.State (Tool).TTY_Image);
         elsif Toolchains.Tool_Is_Configured (Tool) then

            --  This shouldn't happen normally, but it can happen if the user
            --  has just changed the cache location.
            if not Tool_Is_External (Tool) then
               Redeploy_If_Needed (Tool_Milestone (Tool));
            end if;

            --  Add the configured tool release to the solution
            Result := Result.Including
              (Release        => Shared.Release
                 (Target           => Tool_Milestone (Tool),
                  Detect_Externals => Tool_Is_External (Tool)),
               Env            => Root.Platform_Properties,
               Add_Dependency => True,
               Shared         => True);
         else
            Trace.Debug ("Toolchain environment: tool not in solution nor "
                         & "defined by the user: " & Tool.TTY_Image);
         end if;
      end loop;

      return Result;
   end Add_Toolchain;

   ---------------------
   -- Is_In_Toolchain --
   ---------------------

   function Is_In_Toolchain (Release : Releases.Release) return Boolean
   is
      use type Dependencies.Dependency;
   begin
      return Tool_Is_Configured (Release.Name) and then
        Tool_Dependency (Release.Name) = Release.To_Dependency.Value;
   end Is_In_Toolchain;

end Alire.Toolchains.Solutions;
