with Alire;
with Alire.OS_Lib;
with Alire.Query;

with Alr.OS_Lib;
with Alr.Templates;

package body Alr.Checkout is

   --------------------------
   -- Generate_GPR_Builder --
   --------------------------

   procedure Generate_GPR_Builder (Root : Alire.Index.Release) is
      Success : Boolean;
      Needed  : constant Alire.Index.Instance := Alire.Query.Resolve (Root.Depends, Success);
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

   procedure Generate_GPR_Builder (Depends : Alire.Index.Instance; Root : Alire.Index.Release) is
      --  Guard not required, will have been called by the caller to obtain the dependencies
   begin
      Templates.Generate_Gpr (Depends, Root);
   end Generate_GPR_Builder;

   ---------------
   -- To_Folder --
   ---------------

   procedure To_Folder (Projects : Alire.Index.Instance;
                        Parent   : String := OS.Projects_Folder;
                        But      : Alire.Project_Name := "")
   is
   begin
      for R of Projects loop
         if R.Project /= But then
            begin
               R.Checkout (Parent_Folder => Parent);
            exception
               when Alire.File_Error =>
                  --  We'll presume it's already there and OK
                  --  FIXME find out for real, or offer option to squash old one
                  Log ("Skipping checkout for already available " & R.Milestone_Image, Detail);
            end;
         end if;
      end loop;
   end To_Folder;

   ------------------
   -- Working_Copy --
   ------------------

   procedure Working_Copy (R              : Alire.Index.Release;
                           Deps           : Alire.Index.Instance;
                           Parent_Folder  : String;
                           Generate_Files : Boolean := True;
                           If_Conflict    : Policies := Skip)
     				--  FIXME policies not implemented
   is
      Project : constant Alire.Project_Name := R.Project;
      Root    : Alire.Index.Release renames R;
   begin
      if If_Conflict /= Skip then
         raise Program_Error with "Unimplemented";
      end if;

      R.Checkout (Parent_Folder);

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
   exception
      when Alire.File_Error =>
         --  We'll presume it's already there and OK
         Log ("Skipping checkout for already available " & Root.Milestone_Image, Detail);
   end Working_Copy;

end Alr.Checkout;
