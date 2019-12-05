with Ada.Directories;

with Alire.Errors;
with Alire.Directories;
with Alire.OS_Lib.Subprocess;
with Alire.VFS;
with Alire.Utils;             use Alire.Utils;

with GNATCOLL.VFS;

package body Alire.Origins.Deployers.Source_Archive is

   package Dirs renames Ada.Directories;

   ------------
   -- Deploy --
   ------------

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome is
      Archive_Name : constant String := This.Base.Archive_Name;
      Archive_File : constant String := Dirs.Compose (Folder, Archive_Name);
   begin
      Trace.Detail ("Extracting source archive...");
      Unpack (Src_File => Archive_File,
              Dst_Dir  => Folder,
              Delete   => True,
              Move_Up  => True);

      return Outcome_Success;
   end Deploy;

   ------------------
   -- Compute_Hash --
   ------------------

   overriding
   function Compute_Hash (This   : Deployer;
                          Folder : String;
                          Kind   : Hashes.Kinds) return Hashes.Any_Digest is
      Archive_Name : constant String := This.Base.Archive_Name;
      Archive_File : constant String := Dirs.Compose (Folder, Archive_Name);
   begin
      return Hashes.Digest (Hashes.Hash_File (Kind, Archive_File));
   end Compute_Hash;

   -----------
   -- Fetch --
   -----------

   overriding
   function Fetch (This   : Deployer; Folder : String) return Outcome is
      use GNATCOLL.VFS;
      Archive_Name : constant String := This.Base.Archive_Name;
      Archive_File : constant String := Dirs.Compose (Folder, Archive_Name);
   begin
      Trace.Debug ("Creating folder: " & Folder);
      Create (+Folder).Make_Dir;

      Trace.Detail ("Downloading archive: " & This.Base.Archive_URL);

      OS_Lib.Subprocess.Checked_Spawn
        ("wget",
         Empty_Vector &
           This.Base.Archive_URL &
           "-q" &
           "-O" &
           Archive_File);

      return Outcome_Success;
   exception
      when E : others =>
         return Alire.Errors.Get (E);
   end Fetch;

   ------------
   -- Unpack --
   ------------

   procedure Unpack (Src_File : String;
                     Dst_Dir  : String;
                     Delete   : Boolean;
                     Move_Up  : Boolean)
   is

      -----------------------
      -- Check_And_Move_Up --
      -----------------------

      procedure Check_And_Move_Up is
         Contents : constant VFS.Virtual_File_Vector :=
                      VFS.Read_Dir
                        (VFS.New_Virtual_File (VFS.From_FS (Dst_Dir)));
         Success  : Boolean;
      begin
         if Natural (Contents.Length) /= 1 or else
           not Contents.First_Element.Is_Directory
         then
            raise Checked_Error with Errors.Set
              ("Unexpected contents where a single directory was expected: "
               & Dst_Dir);
         end if;

         Trace.Debug ("Unpacked crate root detected as: "
                      & Contents.First_Element.Display_Base_Dir_Name);

         --  Move everything up one level:

         for File of VFS.Read_Dir (Contents.First_Element) loop
            declare
               use type VFS.Virtual_File;
               New_Name : constant VFS.Virtual_File :=
                            Contents.First_Element.Get_Parent /
                              VFS.Simple_Name (File);
            begin
               GNATCOLL.VFS.Rename
                 (File      => File,
                  Full_Name => New_Name,
                  Success   => Success);

               if not Success then
                  raise Checked_Error with Errors.Set
                    ("Could not rename " & File.Display_Full_Name
                     & " to " & New_Name.Display_Full_Name);
               end if;
            end;
         end loop;

         --  Delete the folder, that must be empty:

         Contents.First_Element.Remove_Dir (Success => Success);
         if not Success then
            raise Checked_Error with Errors.Set
              ("Could not remove supposedly empty directory: "
               & Contents.First_Element.Display_Full_Name);
         end if;

      end Check_And_Move_Up;

      package Subprocess renames Alire.OS_Lib.Subprocess;
      use GNATCOLL.VFS;

   begin

      case Archive_Format (Src_File) is
         when Tarball =>

            declare
               --  We had some trouble on Windows with trying to extract a
               --  tar archive in a specified destination directory (using the
               --  -C switch). To workaround these issue we now move to the
               --  destination directory before running tar.

               --  Create the destination directory
               Dst       : constant Virtual_File := Create (+Dst_Dir);
               Dst_Guard : Directories.Temp_File :=
                 Directories.With_Name (+Dst.Full_Name);

               --  Enter the destination directory, and automatically restore
               --  the current dir at the end of the scope.
               Guard : Directories.Guard (Directories.Enter (Dst_Dir))
                 with Unreferenced;
            begin
               Subprocess.Checked_Spawn
                 ("tar", Empty_Vector &
                    "--force-local" &
                    "-xf" & Src_File);

               --  In case of success we keep the destination folder
               Dst_Guard.Keep;
            end;
         when Zip_Archive =>
            Subprocess.Checked_Spawn
              ("unzip", Empty_Vector & "-q" & Src_File & "-d" & Dst_Dir);
         when Unknown =>
            raise Checked_Error with Errors.Set
              ("Given packed archive has unknown format: " & Src_File);
      end case;

      if Delete then
         Trace.Debug ("Deleting source archive: " & Src_File);
         Ada.Directories.Delete_File (Src_File);
      end if;

      if Move_Up then
         Check_And_Move_Up;
      end if;
   end Unpack;

end Alire.Origins.Deployers.Source_Archive;
