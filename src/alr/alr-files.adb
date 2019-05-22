with Ada.Directories;

package body Alr.Files is

   -------------------------
   -- Locate_Any_GPR_File --
   -------------------------

   function Locate_Any_GPR_File return Natural is
      use Ada.Directories;

      Candidates : Utils.String_Vector;

      procedure Check (File : Directory_Entry_Type) is
      begin
         Candidates.Append (Full_Name (File));
      end Check;
   begin
      Search (Current_Directory, "*.gpr", (Ordinary_File => True, others => False), Check'Access);

      return Natural (Candidates.Length);
   end Locate_Any_GPR_File;

   ------------------------
   -- Backup_If_Existing --
   ------------------------

   procedure Backup_If_Existing (File : String) is
      use Ada.Directories;
   begin
      if Exists (File) then
         Trace.Debug ("Backing up " & File);
         Copy_File (File, File & ".prev", "mode=overwrite");
      end if;
   end Backup_If_Existing;

end Alr.Files;
