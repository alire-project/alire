with Ada.Directories;

with Alire.Repositories.Local;

with Alr.Utils;

package body Alr.Project is

   -----------------
   -- Check_Valid --
   -----------------

   procedure Check_Valid is
   begin
      if Current.Is_Empty then
         Log ("No root project defined despite session being current, check project_alr.ads file");
         raise Command_Failed;
      end if;

      if OS_Lib.Locate_Index_File (Current.Element.Project) = "" then
         if OS_Lib.Locate_Any_Index_File /= "" then
            Log ("Session/Project mismatch:");
            Log ("Root project is " & Utils.Quote (Current.Element.Milestone_Image));
            Log ("Session file is " & Utils.Quote (OS_Lib.Locate_Any_Index_File));
         else
            Log ("Could not find a valid session file");
         end if;

         raise Command_Failed;
      end if;
   end Check_Valid;

   ----------------
   -- Enter_Root --
   ----------------

   function Enter_Root (Prj : Alire.Project_Name := Current.Element.Project) return Alire.OS_Lib.Folder_Guard is
      Root_Folder : constant String := OS_Lib.Locate_Above_Project_Folder (Prj);
   begin
      if Root_Folder /= "" then
         if Root_Folder /= OS_Lib.Current_Folder then
            Log ("Using project folder " & Utils.Quote (Root_Folder));
         end if;

         return Alire.OS_Lib.Enter_Folder (Root_Folder);
      else
         Log ("Root folder for project not found", Debug);
         raise Ada.Directories.Use_Error;
      end if;
   end Enter_Root;

   ----------------------
   -- Set_Root_Project --
   ----------------------

   function Set_Root_Project (Project    : Alire.Project_Name;
                              Version    : Semantic_Versioning.Version;
                              Depends_On : Alire.Depends.Dependencies := Alire.Depends.Nothing)
                              return Release
   is
      Rel : constant Release := Alire.Releases.New_Release (Project,
                                                            Version,
                                                            Alire.Repositories.Local.Repo,
                                                            "filesystem",
                                                            Depends_On);
   begin
      Alr.Project.Current.Replace_Element (Rel);

      return Rel;
   end Set_Root_Project;

end Alr.Project;
