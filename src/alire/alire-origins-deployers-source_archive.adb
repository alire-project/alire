with Ada.Directories;

with Alire.Errors;
with Alire.Directories;
with Alire.OS_Lib.Subprocess;
with Alire.OS_Lib.Download;
with Alire.VFS;
with Alire.Utils;             use Alire.Utils;

with GNATCOLL.OS.Constants;
with GNATCOLL.VFS;

package body Alire.Origins.Deployers.Source_Archive is

   package Dirs renames Ada.Directories;

   procedure Untar (Src_File_Full_Name : Absolute_Path);
   --  Extract the tar archive at Src_File_Full_Name in the current directory

   -----------
   -- Untar --
   -----------

   procedure Untar (Src_File_Full_Name : Absolute_Path) is

      package Subprocess renames Alire.OS_Lib.Subprocess;

      use type GNATCOLL.OS.OS_Type;

      Unused : String_Vector;
   begin

      case GNATCOLL.OS.Constants.OS is

         when GNATCOLL.OS.Windows =>

            --  On some versions of tar found on Windows, an option is required
            --  to force e.g. C: to mean a local file location rather than the
            --  net host C. Unfortunatly this not common to all tar on Windows,
            --  so we first try to untar with the --force-local, and if that
            --  fails, we retry without --force-local.

            declare
            begin
               --  Try to untar with --force-local
               Unused := Subprocess.Checked_Spawn_And_Capture
                 ("tar", Empty_Vector &
                    "--force-local" &
                    "-x" &
                    "-f" & Src_File_Full_Name,
                  Err_To_Out => True);

            exception

               when E : Checked_Error =>

                  Trace.Debug ("tar --force-local failed: " & Errors.Get (E));

                  --  In case of error, retry without the --force-local option
                  Unused := Subprocess.Checked_Spawn_And_Capture
                    ("tar", Empty_Vector &
                       "-x" &
                       "-f" & Src_File_Full_Name,
                     Err_To_Out => True);
            end;

         when GNATCOLL.OS.Unix | GNATCOLL.OS.MacOS =>

            --  On other platforms, just run tar without --force-local
            Unused := Subprocess.Checked_Spawn_And_Capture
              ("tar", Empty_Vector &
                 "-x" &
                 "-f" & Src_File_Full_Name,
               Err_To_Out => True);
      end case;

   exception
      when E : Checked_Error =>
         Trace.Debug ("tar failed: " & Errors.Get (E));

         --  Reraise current occurence
         raise;
   end Untar;

   ------------
   -- Deploy --
   ------------

   overriding
   function Deploy (This : Deployer; Folder : String) return Outcome is
      Archive_Name : constant String := This.Base.Archive_Name;
      Archive_File : constant String := Dirs.Compose (Folder, Archive_Name);
   begin
      Trace.Detail ("Deploying source archive " &
                      Archive_Name &
                      " into " &
                      Folder &
                      "...");
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
   function Fetch (This : Deployer; Folder : String) return Outcome is
   begin
      return OS_Lib.Download.File (URL      => This.Base.Archive_URL,
                                   Filename => This.Base.Archive_Name,
                                   Folder   => Folder);
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
         if Natural (Contents.Length) = 0 then
            raise Checked_Error with Errors.Set
              ("No content where a single directory was expected: " & Dst_Dir);
         end if;

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
               --  destination directory before running tar ..

               --  .. which means we need to preserve the full name of
               --  the source file, in case the given name is
               --  relative.
               Src_File_Full_Name : constant Absolute_Path
                 := +GNATCOLL.VFS.Full_Name
                   (GNATCOLL.VFS.Create_From_Base (+Src_File));

               --  Create the destination directory
               Dst       : constant Virtual_File := Create (+Dst_Dir);
               Dst_Guard : Directories.Temp_File :=
                 Directories.With_Name (+Dst.Full_Name);

               --  Enter the destination directory, and automatically restore
               --  the current dir at the end of the scope.
               Guard : Directories.Guard (Directories.Enter (Dst_Dir))
                 with Unreferenced;
            begin

               Untar (Src_File_Full_Name);

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
