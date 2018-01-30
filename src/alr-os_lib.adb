with Ada.Directories;

with GNAT.OS_Lib;

package body Alr.OS_Lib is

   ----------------
   -- Alire_File --
   ----------------

   function Alire_File (Project : Alire.Project_Name) return String is
      (Project & "-alire.ads");

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

end Alr.OS_Lib;
