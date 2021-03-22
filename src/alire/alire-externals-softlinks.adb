with Ada.Directories;

with Alire.URI;
with Alire.Utils.TTY;

with GNATCOLL.VFS;

package body Alire.Externals.Softlinks is

   package Adirs renames Ada.Directories;
   use TOML;

   package Keys is

      --  TOML Keys used locally

      Kind : constant String := "kind";
      Path : constant String := "path";

   end Keys;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (Table : TOML_Adapters.Key_Queue) return External is
      Path : constant String :=
               Table.Checked_Pop (Keys.Path, TOML_String).As_String;
   begin
      return New_Softlink (Path);
   end From_TOML;

   ------------------
   -- New_Softlink --
   ------------------

   function New_Softlink (From  : URL) return External
   is
   begin
      if URI.Scheme (From) not in URI.File_Schemes then
         Raise_Checked_Error
           ("Given URL does not seem to denote a local path: "
            & Utils.TTY.Emph (From));
      end if;

      declare
         Path : constant Any_Path := URI.Local_Path (From);
      begin
         if not GNAT.OS_Lib.Is_Directory (Path) then
            Trace.Warning ("Given path does not exist: "
                           & Utils.TTY.Emph (Path));
         end if;

         --  Store the path as a minimal relative path, so cloning a monorepo
         --  will work as-is, when originally given as a relative path

         declare
            use GNATCOLL.VFS;
            Target : constant Filesystem_String :=
                       (if Check_Absolute_Path (From)
                        then +From
                        else GNATCOLL.VFS.Relative_Path
                          (File => Create (+Adirs.Full_Name (From)),
                           From => Create (+Adirs.Current_Directory)));

         begin
            return (Externals.External with
                    Path_Length => Target'Length,
                    Path        => Any_Path (Target));
         end;
      end;
   end New_Softlink;

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (This : External) return TOML.TOML_Value is
      Table : constant TOML_Value := Create_Table;
   begin
      Table.Set (Keys.Kind,
                 Create_String (Utils.To_Lower_Case (Softlink'Img)));

      Table.Set (Keys.Path,
                 Create_String ("file:" & This.Path));
      --  Ensure file: is there so absolute paths on Windows do not report the
      --  drive letter as the scheme (file:C:\\ is correct, C:\\ is not).

      return Table;
   end To_TOML;

end Alire.Externals.Softlinks;
