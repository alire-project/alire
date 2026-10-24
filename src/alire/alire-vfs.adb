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

   ----------------------------
   -- Error_In_Portable_Path --
   ----------------------------

   function Error_In_Portable_Path (Path : String;
                                    What : String := "path") return String
   is
      function Invalid (Reason : String) return String
      is ("invalid " & What & " '" & Path & "': " & Reason);
   begin
      if (for some C of Path => C = '\') then
         return Invalid ("it must use forward slashes");
      elsif Path /= "" and then Path (Path'First) = '/' then
         return Invalid ("it must be a relative path");
      elsif (for some C of Path => C = ':') then
         return Invalid ("it must not contain ':'");
      else
         return "";
      end if;
   end Error_In_Portable_Path;

   -----------------------------
   -- Error_In_Contained_Path --
   -----------------------------

   function Error_In_Contained_Path (Path : String;
                                     What : String := "path") return String
   is
      Error    : constant String := Error_In_Portable_Path (Path, What);
      Segments : constant AAA.Strings.Vector := Split (Path, '/');
   begin
      if Error /= "" then
         return Error;
      elsif (for some Segment of Segments => Segment = "..") then
         return "invalid " & What & " '" & Path
           & "': it must not contain '..'";
      else
         return "";
      end if;
   end Error_In_Contained_Path;

   --------------------------
   -- Error_In_Rooted_Path --
   --------------------------

   function Error_In_Rooted_Path (Path : String;
                                  Base : String;
                                  What : String := "path") return String
   is
      Error         : constant String := Error_In_Portable_Path (Path, What);
      Base_Segments : constant AAA.Strings.Vector := Split (Base, '/');
      Path_Segments : constant AAA.Strings.Vector := Split (Path, '/');
      Depth         : Integer := 0;
      --  Of the current folder below the root
   begin
      if Error /= "" then
         return Error;
      end if;

      for Segment of Base_Segments loop
         if Segment not in "" | "." then
            Depth := Depth + 1;
         end if;
      end loop;

      for Segment of Path_Segments loop
         if Segment = ".." then
            Depth := Depth - 1;
            if Depth < 0 then
               return "invalid " & What & " '" & Path
                 & "': it must not go above the root of its origin";
            end if;
         elsif Segment not in "" | "." then
            Depth := Depth + 1;
         end if;
      end loop;

      return "";
   end Error_In_Rooted_Path;

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
