with Ada.Directories;

with Alire;
with Alire.Actions;
with Alire.Containers;
with Alire.Externals.Lists;
with Alire.Lockfiles;
with Alire.Origins.Deployers;
with Alire.Roots;

with Alr.Actions;
with Alr.Dependency_Graphs;
with Alr.OS_Lib;
with Alr.Platform;
with Alr.Templates;

package body Alr.Checkout is

   --------------
   -- Checkout --
   --------------

   procedure Checkout (R             : Alire.Releases.Release;
                       Parent_Folder : String;
                       Was_There     : out Boolean;
                       Perform_Actions : Boolean := True)
   is
      use all type Alire.Actions.Moments;
      use Alr.OS_Lib.Paths;
      Folder : constant String := Parent_Folder / R.Unique_Folder;
      Result : Alire.Outcome;
   begin
      if Ada.Directories.Exists (Folder) then
         Was_There := True;
         Trace.Detail ("Skipping checkout of already available " &
                         R.Milestone.Image);
      else
         Was_There := False;
         Trace.Detail ("About to deploy " & R.Milestone.Image);
         Result := Alire.Origins.Deployers.Deploy (R, Folder);
         if not Result.Success then
            Trace.Error (Alire.Message (Result));
            raise Command_Failed;
            --  TODO: this will be eventually refactored into Alire and the
            --  exception removed
         end if;
      end if;

      --  Actions must run always, in case this is a subcrate with shared
      --  folder.
      if Perform_Actions and then Ada.Directories.Exists (Folder) then
         declare
            use OS_Lib;
            Guard : Folder_Guard (Enter_Folder (Folder)) with Unreferenced;
         begin
            Actions.Execute_Actions (R, Post_Fetch);
         end;
      end if;
   end Checkout;

   ------------------
   -- Dependencies --
   ------------------

   procedure Dependencies
     (Root     : Alire.Crate_Name;
      Solution : Alire.Solver.Solution;
      Root_Dir : Alire.Any_Path;
      Deps_Dir : Alire.Absolute_Path := Paths.Dependencies_Folder)
   is
      Was_There : Boolean;
      Graph     : Dependency_Graphs.Graph :=
                    Dependency_Graphs.From_Solution (Solution);
      Pending   : Alire.Solver.Solution   := Solution;
      Round     : Natural                 := 0;
   begin

      --  Notify about missing external dependencies:

      if not Pending.Hints.Is_Empty then
         Trace.Warning
           ("The following external dependencies "
            & "are unavailable within Alire:");
         for Dep of Pending.Hints loop
            Trace.Warning ("   " & Dep.Image);
            for Hint of Alire.Index.Crate (Dep.Crate)
                        .Externals.Hints (Dep.Crate, Platform.Properties)
            loop
               Trace.Warning ("      Hint: " & Hint);
            end loop;
         end loop;
         Trace.Warning
           ("They should be made available in the environment by the user.");
      end if;

      --  Store given solution on disk

      Alire.Lockfiles.Write
        (Solution,
         Platform.Properties,
         Alire.Lockfiles.File_Name (Name     => Root,
                                    Root_Dir => Root_Dir));

      --  Deploy resolved dependencies:

      while not Pending.Releases.Is_Empty loop
         Round := Round + 1;

         declare
            To_Remove : Alire.Containers.Release_Set;
         begin
            --  TODO: this can be done in parallel within each round
            for Rel of Pending.Releases loop
               if Graph.Has_Dependencies (Rel.Name) then
                  Trace.Debug ("Round" & Round'Img & ": SKIP not-ready " &
                                 Rel.Milestone.Image);
               else
                  Trace.Debug ("Round" & Round'Img & ": CHECKOUT ready " &
                                 Rel.Milestone.Image);
                  Graph := Graph.Removing_Dependee (Rel.Name);
                  To_Remove.Include (Rel);
                  if Rel.Name /= Root then
                     Checkout (Rel, Deps_Dir, Was_There);
                  else
                     Trace.Debug
                       ("Skipping checkout of root crate as dependency");
                  end if;
               end if;
            end loop;

            if To_Remove.Is_Empty then
               Trace.Error ("No release checked-out in round" & Round'Img);
               Trace.Error ("Remaining releases:"
                            & Pending.Releases.Length'Img &
                              "; Dependency graph:");
               Graph.Print (Pending.Releases);
               raise Program_Error
                 with "No release checked-out in round" & Round'Img;
            else
               for Rel of To_Remove loop
                  Pending.Releases.Exclude (Rel.Name);
               end loop;
            end if;
         end;
      end loop;

      return;
   end Dependencies;

   ------------------
   -- Working_Copy --
   ------------------

   procedure Working_Copy (R               : Alire.Index.Release;
                           Parent_Folder   : String;
                           Generate_Files  : Boolean := True;
                           Perform_Actions : Boolean := True)
                           --  FIXME policies not implemented
   is
      Was_There : Boolean with Unreferenced;
   begin
      Checkout (R, Parent_Folder, Was_There, Perform_Actions);

      --  And generate its working files, if they do not exist
      if Generate_Files then
         declare
            use OS_Lib;
            Guard      : Folder_Guard (Enter_Folder (R.Unique_Folder))
              with Unreferenced;
            Root       : constant Alire.Roots.Root :=
              Alire.Roots.New_Root
                 (R.Name, Ada.Directories.Current_Directory);
         begin
            --  TODO: be able to export dynamic expressions.
            --  Currently, as workaround, we resolve the release for the
            --  current platform (this was also unimplemented in the old index)
            Templates.Generate_Prj_Alr (R.Whenever (Platform.Properties),
                                        Root.Crate_File);
         end;
      end if;
   end Working_Copy;

end Alr.Checkout;
