with Ada.Directories;
with Ada.Text_IO;

with Alire.Directories;
with Alire.Paths;
with Alire.Solutions;
with Alire.TOML_Adapters;

with TOML.File_IO;

package body Alire.Lockfiles is

   use Directories.Operators;

   ---------------
   -- File_Name --
   ---------------

   function File_Name (Name     : Crate_Name;
                       Root_Dir : Any_Path) return Any_Path is
     (Root_Dir / Paths.Working_Folder_Inside_Root / (+Name) & ".lock");

   ----------
   -- Read --
   ----------

   function Read (Filename : Any_Path) return Solver.Solution is
   begin
      Trace.Debug ("Reading solution from " & Filename);

      declare
         Result : constant TOML.Read_Result :=
                    TOML.File_IO.Load_File (Filename);
      begin
         if Result.Success then
            return Solutions.From_TOML
              (TOML_Adapters.From (Result.Value, Filename & ":"));
         else
            Raise_Checked_Error (TOML.Format_Error (Result));
         end if;
      end;
   end Read;

   --------------
   -- Validity --
   --------------

   function Validity (File : Any_Path) return Validities is
   begin
      if not GNAT.OS_Lib.Is_Read_Accessible_File (File) then
         return Missing;
      end if;

      --  Try to load to assess validity

      declare
         Unused : constant Solver.Solution := Read (File);
      begin
         return Valid;
      end;

   exception
      when E : others =>
         Trace.Debug ("Exception while loading lockfile is: ");
         Log_Exception (E, Debug);
         return Invalid;
   end Validity;

   -----------
   -- Write --
   -----------

   procedure Write (Solution    : Solver.Solution;
                    Environment : Properties.Vector;
                    Filename    : Any_Path)
   is
      pragma Unreferenced (Environment);
      use Ada.Text_IO;
      File : File_Type;
   begin
      Trace.Debug ("Dumping solution to " & Filename);
      Create (File, Out_File, Filename);
      TOML.File_IO.Dump_To_File (Solution.To_TOML, File);
      Close (File);
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;

         --  Clean up

         if Ada.Directories.Exists (Filename) then
            Ada.Directories.Delete_File (Filename);
         end if;

         raise;
   end Write;

end Alire.Lockfiles;
