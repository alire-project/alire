with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Streams.Stream_IO;

with Alire.Errors;

with GNATCOLL.VFS;

package body Alire.Hashes.Common is

   package VFS renames GNATCOLL.VFS;
   use all type VFS.Filesystem_String;

   --  Forward declarations:

   procedure Update (C : in out Context; VF : VFS.Virtual_File);

   procedure Update (C : in out Context; Input : String);

   --------------------
   -- Hash_Directory --
   --------------------

   function Hash_Directory (Path   : Platform_Independent_Path;
                            Except : Platform_Independent_Path := "")
                            return Any_Hash
   is
      use GNATCOLL.VFS;

      function Less_Than_UTF8 (L, R : Virtual_File) return Boolean is
        (Display_Full_Name (L) < Display_Full_Name (R));

      package Sorted_Files is new Ada.Containers.Indefinite_Ordered_Sets
        (Element_Type => Virtual_File,
         "<"          => Less_Than_UTF8);

      Dir_Separator : constant Character := '/';

      Ctxt    : Context;

      Start : constant Virtual_File := Create (+Path);

      -------------
      -- Process --
      -------------

      procedure Process (Parent        : String;
                         File          : Virtual_File) is
         --  Parent stores the route from Path to current file, using utf8
         --  names and '/' as separator. Parent must always end in '/' and is
         --  just '/' for the top level.

         pragma Assert (Parent (Parent'Last) = Dir_Separator);

         Sorted_Entries : Sorted_Files.Set;
         --  Stores the contents of current File, when it is a folder

         ---------------------
         -- Prepare_Entries --
         ---------------------

         procedure Prepare_Entries is
            Entries : File_Array_Access := Read_Dir (File);
         begin
            --  According to VFS docs, VFS.Sort uses native encoding filenames
            --  for sorting. To ensure repeatability, we want to sort on
            --  the utf8 name, so we do our own sorting here. Using a
            --  sorted container this should be O (N log (N)) anyway.

            for E of Entries.all loop
               if Display_Base_Name (E) /= "." and then
                  Display_Base_Name (E) /= ".." and then
                  Display_Base_Name (E) /= Except
               then
                  Sorted_Entries.Insert (E);
               end if;
            end loop;

            Unchecked_Free (Entries);
         end Prepare_Entries;

         ----------------------
         -- Process_Metadata --
         ----------------------

         procedure Process_Metadata is
            --  Name is always included, except for the top-level Path. Then,
            --  a 'f', or 'd' is hashed for files/dirs respectively. For
            --  files, its filesize is hashed; for directories, its entry
            --  count (ommiting '.', '..', Except) is hashed. (Both as
            --  trimmed decimal number strings).

            Kind_Marker : constant array (Boolean) of Character :=
                            (False => 'd',
                             True  => 'f');

            Hash_Name : constant String := Parent &
                     (if Is_Regular_File (File)
                      then Display_Base_Name (File)
                      else Display_Base_Dir_Name (File));
         begin

            --  Name

            if File /= Start then
               Trace.Debug ("Hashing: - name: " & Hash_Name
                            & " (" & Utils.Trim (Hash_Name'Length'Img)
                            & " bytes)");
               Update (Ctxt, Hash_Name);
            end if;

            --  Kind

            Trace.Debug ("Hashing: - kind: "
                         & Kind_Marker (Is_Regular_File (File)));
            Update (Ctxt, "" & Kind_Marker (Is_Regular_File (File)));

            --  Size/Entry count

            if Is_Regular_File (File) then
               declare
                  Size_Img : constant String := Utils.Trim (Size (File)'Img);
               begin
                  Trace.Debug ("Hashing: - size: " & Size_Img);
                  Update (Ctxt, Size_Img);
               end;

            else
               declare
                  Count_Img : constant String :=
                                Utils.Trim (Sorted_Entries.Length'Img);
               begin
                  Trace.Debug ("Hashing: - count: " & Count_Img);
                  Update (Ctxt, Count_Img);
               end;
            end if;
         end Process_Metadata;

      ------------------
      -- Process body --
      ------------------
      --  Deal with a single entry of any kind.

      begin
         Trace.Debug ("Hashing: entry: " & Display_Full_Name (File));

         --  To prevent reiterative checks, get this out of the way now:

         if not (Is_Directory (File) or else Is_Regular_File (File)) then
            raise Checked_Error with Errors.Set
              ("Unsupported directory entry type: "
               & Display_Full_Name (File));
         end if;

         --  To avoid listing twice a folder contents, we start by extracting
         --  and sorting valid folder entries:

         if Is_Directory (File) then
            Prepare_Entries;
         end if;

         Process_Metadata;

         --  Either hash a file or recurse into a subdirectory:

         if Is_Regular_File (File) then
            Trace.Debug ("Hashing: - contents (binary)");
            Update (Ctxt, File);

         else
            --  Do the actual processing in proper order:
            for E of Sorted_Entries loop
               Process (Parent
                        & (if File /= Start
                           then Display_Base_Dir_Name (File) & Dir_Separator
                           else ""),
                        E);
            end loop;
         end if;
      end Process;

   begin
      if not Is_Directory (Start) then
         raise Checked_Error with
           Errors.Set ("Root tree to hash is not a folder: "
                       & Display_Full_Name (Start));
      end if;

      Process (Parent => "" & Dir_Separator,
               File   => Start);

      return New_Hash (Kind, Any_Digest (Digest (Ctxt)));
   end Hash_Directory;

   ---------------
   -- Hash_File --
   ---------------

   function Hash_File (Path : Platform_Independent_Path) return Any_Hash is
      Ctxt : Context;
   begin
      Update (Ctxt, VFS.Create (VFS.Filesystem_String (Path)));

      return Any_Hash (Utils.To_Lower_Case (Kind'Img) & ":" & Digest (Ctxt));
   end Hash_File;

   ------------
   -- Update --
   ------------

   procedure Update (C : in out Context; VF : VFS.Virtual_File) is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;
      use VFS;

      File : File_Type;
      Buff : Stream_Element_Array (1 .. 1024 * 1024); -- 1MiB
      Last : Stream_Element_Offset;
   begin
      Open (File, In_File, +Full_Name (VF));

      while not End_Of_File (File) loop
         Read (File, Buff, Last);
         Update (C, Buff (Buff'First .. Last));
      end loop;

      Close (File);

   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Update;

   ------------
   -- Update --
   ------------

   procedure Update (C : in out Context; Input : String) is
      use Ada.Streams;
      Overlay : Stream_Element_Array (1 .. Input'Length) with
        Address => Input (Input'First)'Address;
      pragma Assert (Character'Size = Stream_Element'Size);
      --  Make explicit our assumption here.
   begin
      Update (C, Overlay);
   end Update;

begin
   Hashes.Hash_File_Functions (Kind) := Hash_File'Access;
end Alire.Hashes.Common;
