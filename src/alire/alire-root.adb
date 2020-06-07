with Alire.Directories;
with Alire.Errors;
with Alire.Paths;
with Alire.TOML_Index;

package body Alire.Root is

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
              (TOML_Index.Load_Release_From_File (File),
               Path);
         exception
            when E : others =>
               Trace.Debug ("Exception while loading crate file is:");
               Log_Exception (E, Debug);

               Trace.Warning ("Could not load crate information from " & File);
               Trace.Warning ("If this workspace was created with a previous"
                              & " alr version you may need to recreate it.");

               return Roots.New_Invalid_Root.With_Reason
                 ("Failed to load " & File & ": " & Errors.Get (E));
         end;
      else
         return Roots.New_Invalid_Root.With_Reason
           ("Could not detect a session folder" &
              " at current or parent locations");
      end if;
   end Current;

end Alire.Root;
