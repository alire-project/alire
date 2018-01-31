with Alire.Repositories.Local;

with Alr.OS_Lib;
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
            Log ("Root project is " & Utils.Quote (Current.Element.Project));
            Log ("Session file is " & Utils.Quote (OS_Lib.Locate_Any_Index_File));
         else
            Log ("Could not find a valid session file");
         end if;

         raise Command_Failed;
      end if;
   end Check_Valid;

   ----------------------
   -- Set_Root_Project --
   ----------------------

   function Set_Root_Project (Project    : Alire.Project_Name;
                              Version    : Semantic_Versioning.Version;
                              Depends_On : Alire.Depends.Dependencies := Alire.Depends.Nothing;
                              License    : Alire.Licenses := Unknown)
                              return Release
   is
      pragma Unreferenced (License);

      Rel : constant Release := Alire.Releases.New_Release (Project,
                                                            Version,
                                                            Alire.Repositories.Local.Repo,
                                                            "filesystem",
                                                            Depends_On);
   begin
      Log ("Root project is " & Rel.Milestone_Image);
      Alr.Project.Current.Replace_Element (Rel);

      return Rel;
   end Set_Root_Project;

end Alr.Project;
