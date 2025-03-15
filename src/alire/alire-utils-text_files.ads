with AAA.Strings;

package Alire.Utils.Text_Files is

   --  A convenience type to hold a complete text file in memory as a vector of
   --  lines. On destruction, changes to the contents are written back to disk.
   --  A backup ".prev" file is also created by default.

   --  Lines are presumed to be UTF-8 and are written via Wide_Wide_Text_IO

   type File (<>) is tagged limited private;

   function Create (Name : Any_Path) return File;

   function Load (From       : Any_Path;
                  Backup     : Boolean := True;
                  Backup_Dir : Any_Path := "")
                  return File;
   --  Load a text file into memory. If Backup, when saving takes place the
   --  original is renamed to ".prev". Backup_Dir optionally designates where
   --  the backup file will be moved.

   function Lines (This : aliased in out File)
                   return access AAA.Strings.Vector;

   function Lines (Filename : Any_Path)
                   return AAA.Strings.Vector;

   procedure Append_Lines (File       : Any_Path;
                           Lines      : AAA.Strings.Vector;
                           Backup     : Boolean  := True;
                           Backup_Dir : Any_Path := "");
   --  Add the given lines to the end of the file

   procedure Replace_Lines (File       : Any_Path;
                            Lines      : AAA.Strings.Vector;
                            Backup     : Boolean  := True;
                            Backup_Dir : Any_Path := "");
   --  Replace contents of File with the new lines.

private

   type File (Length, Backup_Len : Natural) is
     new Ada.Finalization.Limited_Controlled
   with record
      Name       : Any_Path (1 .. Length);
      Lines      : aliased AAA.Strings.Vector; -- The final contents
      Orig       : AAA.Strings.Vector;         -- The original contents
      Backup     : Boolean := True;
      Backup_Dir : Any_Path (1 .. Backup_Len);
   end record;

   overriding
   procedure Finalize (This : in out File);

end Alire.Utils.Text_Files;
