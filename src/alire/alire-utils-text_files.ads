package Alire.Utils.Text_Files is

   --  A convenience type to hold a complete text file in memory as a vector of
   --  lines. On destruction, changes to the contents are written back to disk.
   --  A backup ".prev" file is also created by default.

   type File (<>) is tagged limited private;

   function Load (From       : Any_Path;
                  Backup     : Boolean := True;
                  Backup_Dir : Any_Path := "")
                  return File;
   --  Load a text file into memory. If Backup, when saving takes place the
   --  original is renamed to ".prev". Backup_Dir optionally designates where
   --  the backup file will be moved.

   function Lines (This : aliased in out File) return access String_Vector;

private

   type File (Length, Backup_Len : Natural) is
     new Ada.Finalization.Limited_Controlled
   with record
      Name       : Any_Path (1 .. Length);
      Lines      : aliased String_Vector; -- The final contents
      Orig       : String_Vector;         -- The original contents
      Backup     : Boolean := True;
      Backup_Dir : Any_Path (1 .. Backup_Len);
   end record;

   overriding
   procedure Finalize (This : in out File);

end Alire.Utils.Text_Files;
