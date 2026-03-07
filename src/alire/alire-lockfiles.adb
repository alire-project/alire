with Ada.Directories;
with Ada.Text_IO;

with Alire.Directories;
with Alire.Paths;
with Alire.TOML_Load;

with TOML.File_IO;

package body Alire.Lockfiles is

   use Directories.Operators;

   ----------
   -- Keys --
   ----------

   package Keys is

      --  Key used internally for TOML serialization

      Solution : constant String := "solution";

   end Keys;

   ---------------
   -- File_Name --
   ---------------

   function File_Name (Root_Dir : Absolute_Path) return Absolute_Path
   is (Root_Dir / Paths.Working_Folder_Inside_Root / Simple_Name);

   ---------------
   -- From_TOML --
   ---------------

   overriding
   function From_TOML (This : in out Contents;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome
   is
   begin
      This.Solution :=
        Solutions.From_TOML
          (TOML_Adapters.From
             (From.Checked_Pop
                (Key  => Keys.Solution,
                 Kind => TOML.TOML_Table),
              Keys.Solution));

      From.Report_Extra_Keys;

      return Outcome_Success;
   end From_TOML;

   ----------
   -- Read --
   ----------

   function Read (Root, Filename : Absolute_Path) return Contents is
      --  Enter the workspace root for this lockfile, so any
      --  relative pin paths can be properly resolved.
      use Alire.Directories;
      CWD : Guard (Enter (Root)) with Unreferenced;
   begin
      Trace.Debug ("Reading persistent contents from " & Filename);

      declare
         Result : constant TOML.Read_Result :=
                    TOML.File_IO.Load_File (Filename);
      begin
         if Result.Success then
            return This : Contents do
               Assert (This.From_TOML
                 (TOML_Adapters.From (Result.Value, Filename & ":")));
            end return;
         else
            Raise_Checked_Error (TOML_Load.Format_Error (Filename, Result));
         end if;
      end;
   end Read;

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (This : Contents) return TOML.TOML_Value
   is
   begin
      return Table : constant TOML.TOML_Value := TOML.Create_Table do
         Table.Set (Keys.Solution, This.Solution.To_TOML);
      end return;
   end To_TOML;

   --------------
   -- Validity --
   --------------

   function Validity (Root, File : Absolute_Path) return Validities is
   begin
      if not GNAT.OS_Lib.Is_Read_Accessible_File (File) then
         return Missing;
      end if;

      --  Try to load to assess validity

      declare
         Unused : constant Contents := Read (Root, File);
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

   procedure Write (Contents : Lockfiles.Contents;
                    Filename : Absolute_Path)
   is
      use Ada.Text_IO;
      File  : File_Type;
   begin
      Trace.Debug ("Dumping lockfile contents to " & Filename);

      Directories.Create_Tree (Directories.Parent (Filename));
      Create (File, Out_File, Filename);
      Put_Line (File,
                "# THIS FILE IS GENERATED. DO NOT EDIT.");
      New_Line (File);

      TOML.File_IO.Dump_To_File (Contents.To_TOML, File);
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
