with Ada.Directories;

package body Alire.VFS is

   ----------------------
   -- Attempt_Portable --
   ----------------------

   function Attempt_Portable (Path : Any_Path;
                              From : Any_Path := Directories.Current)
                              return String
   is
      Relative : constant Any_Path :=
         Directories.Find_Relative_Path (Parent => From,
                                         Child  => Path);
   begin
      if Check_Absolute_Path (Relative) then
         return Path;
      else
         return String (To_Portable (Relative));
      end if;
   end Attempt_Portable;

   --------------
   -- Read_Dir --
   --------------

   function Read_Dir
     (Dir     : Virtual_File;
      Filter  : Read_Dir_Filter := All_Files;
      Special : Boolean         := True) return Virtual_File_Vector
   is
      Files : GNATCOLL.VFS.File_Array_Access :=
                Dir.Read_Dir (GNATCOLL.VFS.Read_Dir_Filter (Filter));
   begin
      return Contents : Virtual_File_Vector do
         Contents.Reserve_Capacity (Files'Length);

         for File of Files.all loop
            if Special or else
              (File.Base_Name /= "." and then File.Base_Name /= "..")
            then
               Contents.Append (File);
            end if;
         end loop;

         GNATCOLL.VFS.Unchecked_Free (Files);
      end return;
   end Read_Dir;

   -----------------
   -- Simple_Name --
   -----------------

   function Simple_Name (File : Virtual_File) return Filesystem_String is
     (if File.Is_Directory
      then File.Base_Dir_Name
      else File.Base_Name);

   -----------------
   -- Is_Same_Dir --
   -----------------

   function Is_Same_Dir (P1, P2 : Any_Path) return Boolean is
      use GNAT.OS_Lib;
   begin
      if not Is_Directory (P1) or else not Is_Directory (P2) then
         return False;
      end if;

      --  Attempt a lest costly check first

      declare
         use Ada.Directories;
      begin
         if Full_Name (P1) = Full_Name (P2) then
            return True;
         end if;
      end;

      --  To be absolutely sure, touch a temp file in one of the dirs and
      --  verify whether it exist in the other.

      declare
         Tmp : constant Directories.Temp_File := Directories.In_Dir (P1);
         use Directories.Operators;
      begin
         Directories.Touch (Tmp.Filename);
         return Is_Regular_File (P2
                                 / Ada.Directories.Simple_Name (Tmp.Filename));
      end;
   end Is_Same_Dir;

end Alire.VFS;
