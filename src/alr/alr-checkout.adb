with Ada.Directories;

with Alire;
with Alire.Actions;
with Alire.Containers;
with Alire.Directories;
with Alire.Origins.Deployers;
with Alire.Roots;

with Alr.Actions;
with Alr.Dependency_Graphs;
with Alr.OS_Lib;
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
         Result := Alire.Origins.Deployers.Deploy (R.Origin, Folder);
         if not Result.Success then
            Trace.Error (Alire.Message (Result));
            raise Command_Failed;
            --  TODO: this will be eventually refactored into Alire and the
            --  exception removed
         end if;
      end if;

      --  Actions must run always, in case this is a subproject with shared
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

   ---------------
   -- To_Folder --
   ---------------

   procedure To_Folder (Projects : Query.Instance;
                        Parent   : String := Paths.Projects_Folder)
   is
      Was_There : Boolean;
      Graph     : Dependency_Graphs.Graph :=
                    Dependency_Graphs.From_Instance (Projects);
      Pending   : Query.Instance := Projects;
      Round     : Natural        := 0;
   begin
      while not Pending.Is_Empty loop
         Round := Round + 1;

         declare
            To_Remove : Alire.Containers.Release_Set;
         begin
            --  TODO: this can be done in parallel within each round
            for Rel of Pending loop
               if Graph.Has_Dependencies (Rel.Project) then
                  Trace.Debug ("Round" & Round'Img & ": SKIP not-ready " &
                                 Rel.Milestone.Image);
               else
                  Trace.Debug ("Round" & Round'Img & ": CHECKOUT ready " &
                                 Rel.Milestone.Image);
                  Checkout (Rel, Parent, Was_There);
                  Graph := Graph.Removing_Dependee (Rel.Project);
                  To_Remove.Include (Rel);
               end if;
            end loop;

            if To_Remove.Is_Empty then
               Trace.Error ("No project checked-out in round" & Round'Img);
               Trace.Error ("Remaining projects:" & Pending.Length'Img &
                              "; Dependency graph:");
               Graph.Print (Pending);
               raise Program_Error
                 with "No project checked-out in round" & Round'Img;
            else
               for Rel of To_Remove loop
                  Pending.Exclude (Rel.Project);
               end loop;
            end if;
         end;
      end loop;

      return; -- Old method follows

      --  Two passes: native packages are installed first in case they're a
      --  tool needed by the non-native packages
--        for Pass in 1 .. 2 loop
--           for R of Projects loop
--              if (R.Origin.Is_Native and then Pass = 1) or else
--                 (Pass = 2 and then not R.Origin.Is_Native)
--              then
--                 Checkout (R, Parent, Was_There);
--              end if;
--           end loop;
--        end loop;
   end To_Folder;

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
              Alire.Roots.New_Root (R.Project, Alire.Directories.Current);
         begin
            Templates.Generate_Prj_Alr (R, Root.Crate_File);
            Templates.Generate_Agg_Gpr (Root);
         end;
      end if;
   end Working_Copy;

end Alr.Checkout;
