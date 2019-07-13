with Ada.Directories;

with Alire.OS_Lib.Subprocess;

with GNATCOLL.VFS;

package body Alire.Origins.Deployers.Source_Archive is

   package Dirs renames Ada.Directories;

   ------------
   -- Deploy --
   ------------

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome is
      use GNATCOLL.VFS;
      Archive_Name : constant String := This.Base.Archive_Name;
      Archive_File : constant String := Dirs.Compose (Folder, Archive_Name);
      Exit_Code    :          Integer;
      package Subprocess renames Alire.OS_Lib.Subprocess;
   begin
      Trace.Detail ("Creating folder: " & Folder);
      Create (+Folder).Make_Dir;

      Trace.Detail ("Downloading archive: " & This.Base.Archive_URL);
      Exit_Code := OS_Lib.Subprocess.Spawn
        ("wget", This.Base.Archive_URL & " -q -O " & Archive_File);
      if Exit_Code /= 0 then
         return Outcome_Failure ("wget call failed with code" & Exit_Code'Img);
      end if;

      Trace.Detail ("Extracting source archive...");
      case This.Base.Archive_Format is
         when Alire.Origins.Tarball =>
            Exit_Code := Subprocess.Spawn
              ("tar", "xf " & Archive_File & " -C " & Folder);
         when Alire.Origins.Zip_Archive =>
            Exit_Code := Subprocess.Spawn
              ("unzip", "-q " & Archive_File & " -d " & Folder);
      end case;
      if Exit_Code /= 0 then
         return Outcome_Failure ("extraction failed with code"
                                 & Exit_Code'Img);
      end if;

      return Outcome_Success;
   end Deploy;

end Alire.Origins.Deployers.Source_Archive;
