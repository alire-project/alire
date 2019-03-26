with Ada.Directories;

with Alr.Spawn;

package body Alr.Origins.Source_Archive is

   package D renames Ada.Directories;

   -----------
   -- Fetch --
   -----------

   overriding procedure Fetch (This : Origin; Folder : String) is
      Archive_Name : constant String := This.Base.Archive_Name;
      Archive_File : constant String := D.Compose (Folder, Archive_Name);
   begin
      Trace.Detail ("Creating folder: " & Folder);
      D.Create_Directory (Folder);

      Trace.Detail ("Downloading archive: " & This.Base.Archive_URL);
      Spawn.Command
        ("wget", This.Base.Archive_URL & " -q -O " & Archive_File);

      Trace.Detail ("Extracting source archive...");
      case This.Base.Archive_Format is
         when Alire.Origins.Tarball =>
            Spawn.Command ("tar", "xf " & Archive_File & " -C " & Folder);
         when Alire.Origins.Zip_Archive =>
            Spawn.Command ("unzip", "-q " & Archive_File & " -d " & Folder);
      end case;
   exception
      when others =>
         raise Command_Failed;
   end Fetch;

   -------------
   -- Install --
   -------------

   overriding procedure Install (This : Origin) is
   begin
      raise Program_Error;
   end Install;

end Alr.Origins.Source_Archive;
