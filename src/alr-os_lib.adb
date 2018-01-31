with Ada.Command_Line;
with Ada.Containers;
with Ada.Directories;

with Alr.Utils;

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
     (Project & "_alrbuild.gpr");

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

end Alr.OS_Lib;
