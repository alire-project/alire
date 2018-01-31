with Ada.Command_Line;

with GNAT.OS_Lib;

package body Alr.OS_Lib is

   ----------------
   -- Alire_File --
   ----------------

   function Alire_File (Project : Alire.Project_Name) return String is
     (Project & "-alire.ads");

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
   begin
      if Is_Regular_File (Alire_File (Project)) then
         return Alire_File (Project);
      else
         --  Check subfolders
         declare
            Search : Search_Type;
            Folder : Directory_Entry_Type;
         begin
            Start_Search (Search, Current_Directory, "", (Directory => True, others => False));
            while More_Entries (Search) loop
               Get_Next_Entry (Search, Folder);
               if Simple_Name (Folder) /= "." and then Simple_Name (Folder) /= ".." then
                  if Is_Regular_File (Compose (Full_Name (Folder), Alire_File (Project))) then
                     End_Search (Search);
                     return Compose (Full_Name (Folder), Alire_File (Project));
                  end if;
               end if;
            end loop;
            End_Search (Search);
         end;
      end if;

      return "";
   end Locate_Index_File;

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
