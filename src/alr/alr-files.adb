with Ada.Directories;

package body Alr.Files is

   -------------------------
   -- Locate_Any_GPR_File --
   -------------------------

   function Locate_Any_GPR_File return Natural is
      use Ada.Directories;

      Candidates : AAA.Strings.Vector;

      procedure Check (File : Directory_Entry_Type) is
      begin
         Candidates.Append (Full_Name (File));
      end Check;
   begin
      Search (Current_Directory,
              "*.gpr",
              (Ordinary_File => True, others => False),
              Check'Access);

      return Natural (Candidates.Length);
   end Locate_Any_GPR_File;

end Alr.Files;
