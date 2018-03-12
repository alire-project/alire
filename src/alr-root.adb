with Alr.Files;
with Alr.Utils;

package body Alr.Root is

   -----------------
   -- Check_Valid --
   -----------------

   procedure Check_Valid is
   begin
      if Is_Empty then
         Trace.Error ("No root project defined despite session being current, check project_alr.ads file");
         raise Command_Failed;
      end if;

      if Files.Locate_Index_File (Image) = "" then
         if Files.Locate_Any_Index_File /= "" then
            Trace.Error ("Session/Project mismatch:");
            Trace.Error ("Root project is " & Utils.Quote (Image));
            Trace.Error ("Session file is " & Utils.Quote (Files.Locate_Any_Index_File));
         else
            Trace.Error ("Could not find a valid session file");
         end if;

         raise Command_Failed;
      end if;
   end Check_Valid;

   ----------------
   -- Enter_Root --
   ----------------

   function Enter_Root (Prj : Alire.Name_String := Image) return OS_Lib.Folder_Guard is
      Start_Folder : constant String := OS_Lib.Current_Folder;
      Root_Folder  : constant String := Files.Locate_Above_Project_Folder (Prj);
   begin
      if Root_Folder /= "" then
         if Root_Folder /= Start_Folder then
            Trace.Detail ("Using project folder " & Utils.Quote (Root_Folder));
         end if;

         return OS_Lib.Enter_Folder (Root_Folder);
      else
         Log ("Root folder for project not found", Warning);
         return OS_Lib.Stay_In_Current_Folder;
      end if;
   end Enter_Root;

end Alr.Root;
