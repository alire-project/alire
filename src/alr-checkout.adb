with Ada.Directories;

with Alire;
with Alire.Actions;
with Alire.Roots;

with Alr.Actions;
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
      end if;

      --  Actions must run always, in case this is a subproject with shared folder
      if Ada.Directories.Exists (Folder) then
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
                        Parent   : String := Hardcoded.Projects_Folder)
   is
      Was_There : Boolean;
   begin
      --  Two passes: native packages are installed first in case they're a
      --  tool needed by the non-native packages
      for Pass in 1 .. 2 loop
         for R of Projects loop
            if (R.Origin.Is_Native and then Pass = 1) or else
               (Pass = 2 and then not R.Origin.Is_Native)
            then
               Checkout (R, Parent, Was_There);
            end if;
         end loop;
      end loop;
   end To_Folder;

   ------------------
   -- Working_Copy --
   ------------------

   procedure Working_Copy (R              : Alire.Index.Release;
                           Parent_Folder  : String;
                           Generate_Files : Boolean := True)
     				--  FIXME policies not implemented
   is
      Was_There : Boolean with Unreferenced;
   begin
      Checkout (R, Parent_Folder, Was_There);

      --  And generate its working files, if they do not exist
      if Generate_Files then
         declare
            use OS_Lib;
            Guard      : Folder_Guard (Enter_Folder (R.Unique_Folder)) with Unreferenced;
            Index_File : constant String := Hardcoded.Working_Deps_File;
         begin
            if Is_Regular_File (Index_File) then
               Trace.Detail ("Renaming in-source alr file: " & Index_File);
               Ada.Directories.Copy_File (Index_File, Index_File & ".orig", "mode=overwrite");
               Ada.Directories.Delete_File (Index_File);
            end if;

            Templates.Generate_Prj_Alr (Templates.Released, R);
            Templates.Generate_Agg_Gpr (Alire.Roots.New_Root (R));
         end;
      end if;
   end Working_Copy;

end Alr.Checkout;
