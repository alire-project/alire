with Alire.Directories;
with Alire.TOML_Keys;
with Alire.URI;
with Alire.Utils.TTY;

package body Alire.Externals.Softlinks is

   package TTY renames Alire.Utils.TTY;

   use TOML;

   package Keys is

      --  TOML Keys used locally

      Kind     : constant String := "kind";
      Origin   : constant String := TOML_Keys.Origin; -- Must be the same key
      Path     : constant String := "path";
      Relative : constant String := "relative";
      Remote   : constant String := "remote";

   end Keys;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (Table : TOML_Adapters.Key_Queue) return External is
      Path : constant String :=
               Table.Checked_Pop (Keys.Path, TOML_String).As_String;
      Remote : constant Boolean :=
                 Table.Checked_Pop (Keys.Remote, TOML_Boolean).As_Boolean;
   begin
      if Remote then
         declare
            Origin : Origins.Origin;
         begin
            Origin.From_TOML (Table).Assert;
            return New_Remote (Origin => Origin,
                               Path   => URI.Local_Path (Path));
         end;
      else
         return New_Softlink (URI.Local_Path (Path));
      end if;
   end From_TOML;

   ----------------
   -- New_Remote --
   ----------------

   function New_Remote (Origin : Origins.Origin;
                        Path   : Relative_Path) return External
   is
      Stored_Path : constant Portable_Path := Alire.VFS.To_Portable (Path);
   begin
      return (Externals.External with
              Has_Remote  => True,
              Remote      => (Used => True, Origin => Origin),
              Relative    => True,
              Path_Length => Stored_Path'Length,
              Rel_Path    => Stored_Path);
   end New_Remote;

   ------------------
   -- New_Softlink --
   ------------------

   function New_Softlink (From  : Any_Path) return External
   is
      --  Try to store a relative path whenever possible so lockfiles are
      --  minimally portable across OSes.
      Path : constant Any_Path :=
               Directories.Find_Relative_Path
                 (Parent => Directories.Current,
                  Child  => From);
   begin
      if Check_Absolute_Path (Path) then
         return (Externals.External with
                 Has_Remote  => False,
                 Remote      => <>,
                 Relative    => False,
                 Path_Length => Path'Length,
                 Abs_Path    => Path);
      else
         declare
            Portable_Target : constant Portable_Path :=
                                Alire.VFS.To_Portable (Path);
         begin
            return (Externals.External with
                    Has_Remote  => False,
                    Remote      => <>,
                    Relative    => True,
                    Path_Length => Portable_Target'Length,
                    Rel_Path    => Portable_Target);
         end;
      end if;
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
      Table.Set (Keys.Remote,
                 Create_Boolean (This.Has_Remote));
      Table.Set (Keys.Relative,
                 Create_Boolean (This.Relative));

      if This.Has_Remote then
         Table.Set (Keys.Origin,
                    This.Remote.Origin.To_TOML);
      end if;

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

   -----------------------
   -- TTY_Relative_Path --
   -----------------------

   function TTY_Relative_Path (This : External) return String
   is (TTY.URL (Directories.Find_Relative_Path (Parent => Directories.Current,
                                                Child  => This.Path)));

end Alire.Externals.Softlinks;
