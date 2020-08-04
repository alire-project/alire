with TOML;

package Alire.Utils.Text_Files is

   --  A convenience type to hold a complete text file in memory as a vector of
   --  lines. On destruction, changes to the contents are written back to disk.
   --  A backup ".prev" file is also created by default.

   type File (<>) is tagged limited private;

   function Load (From   : Any_Path;
                  Backup : Boolean := True)
                  return File;
   --  Load a text file into memory. If Backup, when saving takes place the
   --  original is renamed to ".prev".

   function Lines (This : aliased in out File) return access String_Vector;

private

   type File (Length : Natural) is new Ada.Finalization.Limited_Controlled
   with record
      Name   : Any_Path (1 .. Length);
      Lines  : aliased String_Vector; -- The final contents
      Orig   : String_Vector;         -- The original contents
      Backup : Boolean := True;
   end record;

   overriding
   procedure Finalize (This : in out File);

end Alire.Utils.Text_Files;
