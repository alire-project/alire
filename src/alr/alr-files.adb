with Alr.Hardcoded;
with Alr.OS_Lib;

package body Alr.Files is

   use OS_Lib.Paths;

   -------------------------
   -- Is_Candidate_Folder --
   -------------------------

   function Is_Candidate_Folder (Folder : String := Ada.Directories.Current_Directory) return Boolean is
     (Ada.Directories.Exists (Folder / Hardcoded.Alr_Working_Folder) and then
      Ada.Directories.Exists (Folder / Hardcoded.Working_Deps_File));

   -----------------------
   -- Locate_File_Under --
   -----------------------

   function Locate_File_Under (Folder    : String;
                               Name      : String;
                               Max_Depth : Natural := Natural'Last) return Utils.String_Vector is
      Found : Utils.String_Vector;

      procedure Locate (Folder : String; Current_Depth : Natural; Max_Depth : Natural) is
         use Ada.Directories;
         Search : Search_Type;
      begin
         Start_Search (Search, Folder, "", Filter => (Ordinary_File => True, Directory => True, others => False));

         while More_Entries (Search) loop
            declare
               Current : Directory_Entry_Type;
            begin
               Get_Next_Entry (Search, Current);
               if Kind (Current) = Directory then
                  if Simple_Name (Current) /= "." and then Simple_Name (Current) /= ".." and then Current_Depth < Max_Depth then
                     Locate (Folder / Simple_Name (Current), Current_Depth + 1, Max_Depth);
                  end if;
               elsif Kind (Current) = Ordinary_File and then Simple_Name (Current) = Simple_Name (Name) then
                  Found.Append (Folder / Name);
               end if;
            end;
         end loop;

         End_Search (Search);
      end Locate;

   begin
      Locate (Folder, 0, Max_Depth);
      return Found;
   end Locate_File_Under;

   -------------------------
   -- Locate_Any_GPR_File --
   -------------------------

   function Locate_Any_GPR_File return Natural is
      use Ada.Directories;

      Candidates : Utils.String_Vector;

      procedure Check (File : Directory_Entry_Type) is
      begin
         Candidates.Append (Full_Name (File));
      end Check;
   begin
      Search (Current_Directory, "*.gpr", (Ordinary_File => True, others => False), Check'Access);

      return Natural (Candidates.Length);
   end Locate_Any_GPR_File;

   ---------------------------------
   -- Locate_Above_Project_Folder --
   ---------------------------------

   function Locate_Above_Project_Folder return String is
      use Ada.Directories;
      use Alr.OS_Lib;

      Guard : Folder_Guard (Enter_Folder (Current_Directory)) with Unreferenced;
   begin
      Trace.Debug ("Starting root search at " & Current_Folder);
      loop
         if Is_Candidate_Folder then
            return Current_Folder;
         else
            Set_Directory (Containing_Directory (Current_Folder));
            Trace.Debug ("Going up to " & Current_Folder);
         end if;
      end loop;
   exception
      when Use_Error =>
         return ""; -- There's no containing folder (hence we're at root)
   end Locate_Above_Project_Folder;

   ------------------------
   -- Backup_If_Existing --
   ------------------------

   procedure Backup_If_Existing (File : String) is
      use Ada.Directories;
   begin
      if Exists (File) then
         Trace.Debug ("Backing up " & File);
         Copy_File (File, File & ".prev", "mode=overwrite");
      end if;
   end Backup_If_Existing;

end Alr.Files;
