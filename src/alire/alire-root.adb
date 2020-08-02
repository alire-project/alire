with Alire.Directories;
with Alire.Errors;
with Alire.Manifest;
with Alire.Paths;
with Alire.Releases;

package body Alire.Root is

   -------------
   -- Current --
   -------------

   function Current return Roots.Optional.Root
   is (Roots.Optional.Detect_Root (Directories.Detect_Root_Path));

   -------------
   -- Current --
   -------------

   function Current return Roots.Root is
      use Alire.Directories;
      Path      : constant String := Directories.Detect_Root_Path;
   begin
      if Path /= "" then
         declare
            File    : constant String :=
              Directories.Find_Single_File
                (Path      => Path / Paths.Working_Folder_Inside_Root,
                 Extension => Paths.Crate_File_Extension_With_Dot);
         begin
            return Roots.New_Root
              (Releases.From_Manifest (File, Manifest.Local),
               Path,
               Platform_Properties);
         exception
            when E : others =>
               Raise_Checked_Error
                 ("Failed to load " & File & ": " & Errors.Get (E));
         end;
      else
         Raise_Checked_Error
           ("Could not detect a session folder" &
              " at current or parent locations");
      end if;
   end Current;

   Environment : Properties.Vector;

   function Platform_Properties return Properties.Vector
   is (Environment);

   procedure Set_Platform_Properties (Env : Properties.Vector) is
   begin
      Environment := Env;
   end Set_Platform_Properties;

end Alire.Root;
