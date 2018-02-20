with Ada.Containers;
with Ada.Directories;

with Alr.Hardcoded;
with Alr.OS_Lib;

with GNAT.OS_Lib;

package body Alr.Files is

   use Ada.Containers;

   function "/" (L, R : String) return String is (Ada.Directories.Compose (L, R));

   -----------------------
   -- Locate_File_Under --
   -----------------------

   function Locate_File_Under (Folder : String; Name : String; Max_Depth : Natural := 0) return Utils.String_Vector is
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
               elsif Kind (Current) = Ordinary_File and then Simple_Name (Current) = Name then
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

   -----------------------
   -- Locate_Index_File --
   -----------------------

   function Locate_Index_File (Project : Alire.Project_Name) return String is
      use Ada.Directories;
      use Gnat.OS_Lib;

      Candidates : Utils.String_Vector;
   begin
      if Is_Regular_File (Hardcoded.Alire_File (Project)) then
         Candidates.Append (Hardcoded.Alire_File (Project));
      end if;

      --  Check subfolders
      declare
         Search : Search_Type;
         Folder : Directory_Entry_Type;
      begin
         Start_Search (Search, Current_Directory, "", (Directory => True, others => False));

         while More_Entries (Search) loop
            Get_Next_Entry (Search, Folder);

            if Simple_Name (Folder) /= "." and then Simple_Name (Folder) /= ".." then
               if Is_Regular_File (Full_Name (Folder) / Hardcoded.Alire_File (Project)) then
                  Candidates.Append (Full_Name (Folder) / Hardcoded.Alire_File (Project));
               end if;
            end if;
         end loop;

         End_Search (Search);
      end;

      if Candidates.Length > 1 then
         Log ("Warning: more than one " & Hardcoded.Alire_File (Project) & " in scope.");
         for C of Candidates loop
            Log (C);
         end loop;
      end if;

      if Candidates.Length = 1 then
         return Candidates.First_Element;
      else
         return "";
      end if;
   end Locate_Index_File;

   ---------------------------
   -- Locate_Any_Index_File --
   ---------------------------

   function Locate_Any_Index_File return String is
      use Ada.Directories;
      use Gnat.OS_Lib;

      Candidates : Utils.String_Vector;

      ---------------
      -- Search_In --
      ---------------

      procedure Search_In (Folder : String) is
         procedure Check (File : Directory_Entry_Type) is
         begin
            Candidates.Append (Full_Name (File));
         end Check;
      begin
         Search (Folder, "*_alr.ads", (Ordinary_File => True, others => False), Check'Access);
      end Search_In;

      ------------------
      -- Check_Folder --
      ------------------

      procedure Check_Folder (Folder : Directory_Entry_Type) is
      begin
         if Simple_Name (Folder) /= "." and then Simple_Name (Folder) /= ".." then
            Search_In (Full_Name (Folder));
         end if;
      end Check_Folder;

   begin
      --  Regular files in current folder
      Search_In (Current_Directory);

      --  Find direct subfolders and look there
      Search (Current_Directory, "", (Directory => True, others => False), Check_Folder'Access);

      if Candidates.Length > 1 then
         Log ("Warning: more than one alr project file in scope.");
         for C of Candidates loop
            Log (C);
         end loop;
      end if;

      if Candidates.Length = 1 then
         return Candidates.First_Element;
      else
         return "";
      end if;
   end Locate_Any_Index_File;

   -------------------------
   -- Locate_Any_GPR_File --
   -------------------------

   function Locate_Any_GPR_File return Natural is
      use Ada.Directories;
      use Gnat.OS_Lib;

      Candidates : Utils.String_Vector;

      procedure Check (File : Directory_Entry_Type) is
      begin
         Candidates.Append (Full_Name (File));
      end Check;
   begin
      Search (Current_Directory, "*.gpr", (Ordinary_File => True, others => False), Check'Access);

      return Natural (Candidates.Length);
   end Locate_Any_GPR_File;

   ---------------------------
   -- Locate_Project_Folder --
   ---------------------------

   function Locate_Above_Project_Folder (Project : Alire.Project_Name) return String is
      use Ada.Directories;
      use Alr.OS_Lib;
      use GNAT.OS_Lib;

      Guard : constant Folder_Guard := Enter_Folder (Current_Directory) with Unreferenced;
   begin
      loop
         if Is_Regular_File (Hardcoded.Project_File (Project)) and then Locate_Index_File (Project) /= "" then
            return Current_Folder;
         else
            Set_Directory (Containing_Directory (Current_Directory));
         end if;
      end loop;
   exception
      when Use_Error =>
         return ""; -- There's no containing folder (hence we're at root)
   end Locate_Above_Project_Folder;

end Alr.Files;
