with Ada.Command_Line;
with Ada.Containers;

with GNAT.OS_Lib;

package body Alr.OS_Lib is

   use type Ada.Containers.Count_Type;

   ----------------
   -- Alire_File --
   ----------------

   function Alire_File (Project : Alire.Project_Name) return String is
     (Project & "_alr.ads");

   ----------------
   -- Build_File --
   ----------------

   function Build_File (Project : Alire.Project_Name) return String is
     (Project & "_alr.gpr");

   ------------------
   -- Project_File --
   ------------------

   function Project_File (Project : Alire.Project_Name) return String is
     (Project & ".gpr");

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
      if Is_Regular_File (Alire_File (Project)) then
         Candidates.Append (Alire_File (Project));
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
               if Is_Regular_File (Full_Name (Folder) / Alire_File (Project)) then
                  Candidates.Append (Full_Name (Folder) / Alire_File (Project));
               end if;
            end if;
         end loop;

         End_Search (Search);
      end;

      if Candidates.Length > 1 then
         Log ("Warning: more than one " & Alire_File (Project) & " in scope.");
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
      use GNAT.OS_Lib;

      Guard : constant Alire.OS_Lib.Folder_Guard := Alire.OS_Lib.Enter_Folder (Current_Directory) with Unreferenced;
   begin
      loop
         if Is_Regular_File (Project_File (Project)) and then Locate_Index_File (Project) /= "" then
            return Current_Folder;
         else
            Set_Directory (Containing_Directory (Current_Directory));
         end if;
      end loop;
   exception
      when Use_Error =>
         return ""; -- There's no containing folder (hence we're at root)
   end Locate_Above_Project_Folder;

   --------------------------
   -- Current_Command_Line --
   --------------------------

   function Current_Command_Line return String is
      use Ada.Command_Line;

      function Append (Arg : Positive) return String is
      begin
         if Arg > Argument_Count then
            return "";
         else
            return Argument (Arg) & " " & Append (Arg + 1);
         end if;
      end Append;

   begin
      return Append (1);
   end Current_Command_Line;

   ---------------------
   -- Traverse_Folder --
   ---------------------

   procedure Traverse_Folder (Folder : String;
                              Doing  : access procedure (Item : Ada.Directories.Directory_Entry_Type))
   is
      use Ada.Directories;
   begin
      Log ("Traversing folder: " & Folder, Debug);
      Search (Folder, "", (Directory => True, Ordinary_File => True, others => False), Doing);
   end Traverse_Folder;

   ----------
   -- Copy --
   ----------

   procedure Copy (Src_Folder, Dst_Parent_Folder : String) is
   begin
      -- FIXME this is OS dependent and should be made independent (or moved to OS)
      -- FIXME this is not robust with blanks in paths
      Alire.OS_Lib.Spawn ("cp", "-r " & Src_Folder& " " & Dst_Parent_Folder);
   end Copy;


   ----------------
   -- Sed_Folder --
   ----------------

   procedure Sed_Folder (Folder  : String;
                         Pattern : String;
                         Replace : String)
   is

      ------------
      -- Rename --
      ------------

      procedure Rename (Item : Ada.Directories.Directory_Entry_Type) is
         use Ada.Directories;
         use Utils;
      begin
         if Simple_Name (Item) = "." or else Simple_Name (Item) = ".." then
            return;
         end if;

         if Kind (Item) = Directory then
            Traverse_Folder (Full_Name (Item), Rename'Access);
         end if;

         if Contains (Simple_Name (Item), Pattern) then
            Log ("Filename match: " & Simple_Name (Item), Debug);
            Rename (Full_Name (Item),
                    Containing_Directory (Full_Name (Item)) / Utils.Replace (Simple_Name (Item), Pattern, Replace));
         end if;
      end Rename;

   begin
      -- FIXME this is OS dependent and should be made independent (or moved to OS)
      --  File contents
      declare
         Guard : constant Alire.OS_Lib.Folder_Guard := Alire.OS_Lib.Enter_Folder (Folder) with Unreferenced;
      begin
         Log ("sed-ing project name in files...", Debug);
         Alire.OS_Lib.Spawn ("find", ". -type f -exec sed -i s/" & Pattern & "/" & Replace & "/g {} \;");
      end;

      --  This is not OS dependent
      --  File names
      Log ("sed-ing project in file names...", Debug);
      Traverse_Folder (Folder, Rename'Access);
   end Sed_Folder;

   -------------------------------
   -- File_Contains_Ignore_Case --
   -------------------------------

   function File_Contains_Ignore_Case (Filename, Word : String) return Boolean is
   begin
      --  FIXME: this is OS dependent, and it shouldn't be
      return Alire.OS_Lib.Spawn ("grep", "-q " & Word & " " & Filename) = 0;
   end File_Contains_Ignore_Case;

end Alr.OS_Lib;
