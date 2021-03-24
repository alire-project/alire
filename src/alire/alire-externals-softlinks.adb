with Alire.URI;
with Alire.Utils.TTY;

with GNATCOLL.VFS;

package body Alire.Externals.Softlinks is

   package Adirs renames Ada.Directories;
   use TOML;

   package Keys is

      --  TOML Keys used locally

      Kind     : constant String := "kind";
      Path     : constant String := "path";
      Relative : constant String := "relative";

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

         --  Store the path as a minimal relative path, so cloning a monorepo
         --  will work as-is, when originally given as a relative path

         declare
            use GNATCOLL.VFS;
            Target : constant Filesystem_String :=
                       (if Check_Absolute_Path (Path)
                        then +Path
                        else GNATCOLL.VFS.Relative_Path
                          (File => Create (+Adirs.Full_Name (Path)),
                           From => Create (+Adirs.Current_Directory)));

         begin
            if Check_Absolute_Path (Path) then
               return (Externals.External with
                       Relative    => False,
                       Path_Length => Path'Length,
                       Abs_Path    => Path);
            else
               return (Externals.External with
                       Relative    => True,
                       Path_Length => Target'Length,
                       Rel_Path    => Alire.VFS.To_Portable (+Target));
            end if;
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
      Table.Set (Keys.Relative,
                 Create_Boolean (This.Relative));

      if This.Relative then
         Table.Set (Keys.Path,
                    Create_String ("file:" & String (This.Rel_Path)));
      else
         Table.Set (Keys.Path,
                    Create_String ("file:" & This.Abs_Path));
      end if;
      --  "file:" is there so absolute paths on Windows do not report the drive
      --  letter as the scheme (file:C:\\ is correct, C:\\ is not).

      return Table;
   end To_TOML;

end Alire.Externals.Softlinks;
