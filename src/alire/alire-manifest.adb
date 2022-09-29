with AAA.Strings;

with Alire.Paths;
with Alire.Releases;
with Alire.TOML_Keys;
with Alire.Utils.Text_Files;

with TOML_Slicer;

package body Alire.Manifest is

   ------------
   -- Append --
   ------------

   procedure Append (Name : Any_Path;
                     Dep  : Dependencies.Dependency)
   is
   begin
      Utils.Text_Files.Append_Lines
        (File       => Name,
         Lines      =>
           AAA.Strings.Empty_Vector
             .Append ("")
             .Append ("[[" & TOML_Keys.Depends_On & "]]")
             .Append (Dep.Manifest_Image),
           Backup     => False);
      --  No need to backup, as this is done already on a copy of the manifest

   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (File  : Any_Path;
                     Crate : Crate_Name;
                     Pin   : User_Pins.Pin)
   is
   begin
      Utils.Text_Files.Append_Lines
        (File       => File,
         Lines      =>
           AAA.Strings.Empty_Vector
             .Append ("")
             .Append ("[[" & TOML_Keys.Pins & "]]")
             .Append (Pin.To_Manifest_Line (Crate)),
         Backup     => False);
      --  No need to backup as this is done on a copy of the manifest already
   end Append;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Name : Any_Path; Source : Sources) return Boolean is
   begin
      --  Check we are able to load the manifest file
      if Releases.From_Manifest
        (Name, Source, Strict => False).Version_Image /= ""
      then
         Trace.Debug ("Checked valid manifest at " & Name);
         return True;
      else
         raise Program_Error with "A release will always have a version";
         return False;
      end if;
   exception
      when E : others =>
         Trace.Debug ("Exception trying to load manifest:");
         Log_Exception (E);
         return False;
   end Is_Valid;

   ------------
   -- Remove --
   ------------

   procedure Remove (Name : Any_Path;
                     Dep  : Crate_Name)
   is
   begin
      TOML_Slicer.Remove_Line_From_Array
        (File_Name  => Name,
         Array_Name => TOML_Keys.Depends_On,
         Entry_Name => Dep.As_String,
         Cleanup    => True,
         Backup     => True,
         Backup_Dir => Paths.Working_Folder_Inside_Root);
   end Remove;

   ----------------
   -- Remove_Pin --
   ----------------

   procedure Remove_Pin (File : Any_Path;
                         Pin  : Crate_Name)
   is
   begin
      TOML_Slicer.Remove_Line_From_Array
        (File_Name  => File,
         Array_Name => TOML_Keys.Pins,
         Entry_Name => Pin.As_String,
         Cleanup    => True,
         Backup     => True,
         Backup_Dir => Paths.Working_Folder_Inside_Root);
   end Remove_Pin;

end Alire.Manifest;
