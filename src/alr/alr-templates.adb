
with Alire.GPR;
with Alire.Milestones;
with Alire.Properties.Labeled; use all type Alire.Properties.Labeled.Labels;
with Alire.Releases.TOML_IO;
with Alire.Utils;

with Alr.Commands;
with Alr.Files;
with Alr.OS_Lib;
with Alr.Root;

with GNATCOLL.VFS;

package body Alr.Templates is

   ----------------------------
   -- Generate_Project_Alire --
   ----------------------------

   procedure Generate_Prj_Alr (Release  : Types.Release;
                               Filename : String)
   is
      use GNATCOLL.VFS;
      F : constant Virtual_File := Create (+Filename, Normalize => True);
   begin
      Trace.Debug ("Generating " & Release.Name_Str & ".toml file for "
                   & Release.Milestone.Image & " with"
                   & Release.Dependencies.Leaf_Count'Img & " dependencies");

      --  Ensure working folder exists (might not upon first get)
      F.Get_Parent.Make_Dir;

      Files.Backup_If_Existing (Filename);

      Alire.Releases.TOML_IO.To_File (Release, Filename);
   end Generate_Prj_Alr;

   ----------------------
   -- Generate_Prj_Alr --
   ----------------------

   procedure Generate_Prj_Alr (Release : Types.Release) is
      Guard : OS_Lib.Folder_Guard (Commands.Enter_Working_Folder)
         with Unreferenced;
   begin
      Generate_Prj_Alr (Release, Root.Current.Crate_File);
   end Generate_Prj_Alr;

end Alr.Templates;
