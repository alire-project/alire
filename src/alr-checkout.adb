with Ada.Directories;

with Alire;
with Alire.Actions;
with Alire.Roots;

with Alr.Actions;
with Alr.Files;
with Alr.OS_Lib;
with Alr.Origins;
with Alr.Templates;

package body Alr.Checkout is

   --------------
   -- Checkout --
   --------------

   procedure Checkout (R             : Alire.Releases.Release;
                       Parent_Folder : String;
                       Was_There     : out Boolean)
   is
      use all type Alire.Actions.Moments;
      use Alr.OS_Lib.Paths;
      Folder : constant String := Parent_Folder / R.Unique_Folder;
   begin
      if Ada.Directories.Exists (Folder) then
         Was_There := True;
         Trace.Detail ("Skipping checkout of already available " & R.Milestone.Image);
      else
         Was_There := False;
         Trace.Detail ("About to deploy " & R.Milestone.Image);
         Alr.Origins.Fetch_Or_Install (R.Origin, Folder);

         if Ada.Directories.Exists (Folder) then
            declare
               use OS_Lib;
               Guard : constant Folder_Guard := Enter_Folder (Folder) with Unreferenced;
            begin
               Actions.Execute_Actions (R, Post_Fetch);
            end;
         end if;
      end if;
   end Checkout;

   ---------------
   -- To_Folder --
   ---------------

   procedure To_Folder (Projects : Query.Instance;
                        Parent   : String := Hardcoded.Projects_Folder)
   is
      Was_There : Boolean;
   begin
      for R of Projects loop
         Checkout (R, Parent, Was_There);
      end loop;
   end To_Folder;

   ------------------
   -- Working_Copy --
   ------------------

   procedure Working_Copy (R              : Alire.Index.Release;
                           Deps           : Query.Instance;
                           Parent_Folder  : String;
                           Generate_Files : Boolean := True;
                           If_Conflict    : Policies := Skip)
     				--  FIXME policies not implemented
   is
      Project : constant Alire.Project := R.Project;
      Root    : Alire.Index.Release renames R;
      Was_There : Boolean with Unreferenced;
   begin
      if If_Conflict /= Skip then
         raise Program_Error with "Unimplemented";
      end if;

      Checkout (R, Parent_Folder, Was_There);

      --  And generate its working files, if they do not exist
      if Generate_Files then
         declare
            use OS_Lib;
            Guard      : Folder_Guard    := Enter_Folder (Root.Unique_Folder) with Unreferenced;
            Index_File : constant String := Files.Locate_Given_Metadata_File (Project);
         begin
            if Index_File /= "" then
               Trace.Detail ("Renaming in-source alr file: " & Index_File);
               Ada.Directories.Copy_File (Index_File, Index_File & ".orig", "mode=overwrite");
               Ada.Directories.Delete_File (Index_File);
            end if;

            Templates.Generate_Prj_Alr (Deps, Alire.Roots.New_Root (Root), Templates.Unknown);
            Templates.Generate_Agg_Gpr (Deps, Alire.Roots.New_Root (Root));
         end;
      end if;
   end Working_Copy;

end Alr.Checkout;
