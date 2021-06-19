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

end Alire.VFS;
