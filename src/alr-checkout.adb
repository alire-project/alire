with Ada.Directories;

with Alire;
with Alire.OS_Lib;
with Alire.Releases;

with Alr.Origins;
with Alr.OS_Lib;
with Alr.Templates;

package body Alr.Checkout is

   --------------
   -- Checkout --
   --------------

   procedure Checkout (R             : Alire.Releases.Release;
                       Parent_Folder : String;
                       Was_There     : out Boolean)
   is
      use Alr.OS_Lib.Paths;
      Folder : constant String := Parent_Folder / R.Unique_Folder;
   begin
      if Ada.Directories.Exists (Folder) then
         Was_There := True;
         Trace.Detail ("Skipping checkout of already available " & R.Milestone_Image);
      else
         Was_There := False;
         Trace.Detail ("About to check out " & R.Milestone_Image);
         Alr.Origins.Fetch (R.Origin, Folder);
      end if;
   end Checkout;

   --------------------------
   -- Generate_GPR_Builder --
   --------------------------

   procedure Generate_GPR_Builder (Root : Alire.Index.Release) is
      Success : Boolean;
      Needed  : constant Alire.Query.Instance := Alire.Query.Resolve (Root.Depends, Success);
   begin
      if Success then
         Generate_GPR_Builder (Needed, Root);
      else
         raise Command_Failed;
      end if;
   end Generate_GPR_Builder;

   --------------------------
   -- Generate_GPR_Builder --
   --------------------------

   procedure Generate_GPR_Builder (Depends : Alire.Query.Instance; Root : Alire.Index.Release) is
      --  Guard not required, will have been called by the caller to obtain the dependencies
   begin
      Templates.Generate_Gpr (Depends, Root);
   end Generate_GPR_Builder;

   ---------------
   -- To_Folder --
   ---------------

   procedure To_Folder (Projects : Alire.Query.Instance;
                        Parent   : String := OS.Projects_Folder;
                        But      : Alire.Project_Name := "")
   is
      Was_There : Boolean;
   begin
      for R of Projects loop
         if R.Project /= But then
            Checkout (R, Parent, Was_There);
         end if;
      end loop;
   end To_Folder;

   ------------------
   -- Working_Copy --
   ------------------

   procedure Working_Copy (R              : Alire.Index.Release;
                           Deps           : Alire.Query.Instance;
                           Parent_Folder  : String;
                           Generate_Files : Boolean := True;
                           If_Conflict    : Policies := Skip)
     				--  FIXME policies not implemented
   is
      Project : constant Alire.Project_Name := R.Project;
      Root    : Alire.Index.Release renames R;
      Was_There : Boolean;
   begin
      if If_Conflict /= Skip then
         raise Program_Error with "Unimplemented";
      end if;

      Checkout (R, Parent_Folder, Was_There);

      --  And generate its working files, if they do not exist
      if Generate_Files and then not R.Is_Native then
         declare
            use Alire.OS_Lib;
            Guard      : Folder_Guard    := Enter_Folder (Root.Unique_Folder) with Unreferenced;
            Index_File : constant String := Alr.OS_Lib.Locate_Index_File (Project);
         begin
            if Index_File = "" then
               Templates.Generate_Project_Alire (Deps, Root);
            end if;

            Templates.Generate_Gpr (Deps, Root);
         end;
      end if;
   end Working_Copy;

end Alr.Checkout;
