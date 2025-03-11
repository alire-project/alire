with Ada.Characters.Latin_1;

with Alire.Directories;
with Alire.Errors;
with Alire.Utils.Text_Files;
with Alire.VFS;

with Den;

package body Alire.Templates is

   ------------
   -- Append --
   ------------

   function Append (This : Tree;
                    File : Portable_Path;
                    Data : Filesystem_Entry) return Tree
   is
   begin
      return Result : Tree := This do
         Result.Insert (File, Data);
      end return;
   end Append;

   ---------------
   -- Append_If --
   ---------------

   function Append_If (This : Tree;
                       Cond : Boolean;
                       File : Portable_Path;
                       Data : Filesystem_Entry) return Tree
   is (if Cond
       then This.Append (File, Data)
       else This);

   ---------------
   -- As_String --
   ---------------

   function As_String (Data : Embedded) return String is
      Fake_String : aliased String (1 .. Data'Length);
      for Fake_String'Address use Data (Data'First)'Address;
   begin
      return Fake_String;
   end As_String;

   --------------------
   -- Translate_File --
   --------------------

   procedure Translate_File (Src : Embedded;
                             Dst : Relative_File;
                             Map : Translations)
   is

      -------------------
      -- Parsing_Error --
      -------------------

      procedure Parsing_Error (Tag_Name : String;
                               Filename : String := "";
                               Line     : Natural := 0;
                               Reason   : Templates_Parser.Reason_Kind)
      is
      begin
         if Reason in Templates_Parser.Unused then
            return;
            --  Unused tags are usual e.g. for no-skel initialization
         end if;

         raise Program_Error with
           Errors.Set
             ("bad tag during generation of " & Filename
              & " at line" & Line'Image
              & ", offending tag is " & Tag_Name
              & ", reason is " & Reason'Image);
      end Parsing_Error;

      Temp : Directories.Temp_File; -- temporary template on disk
   begin
      --  To gain access to the full capabilities of Templates_Parser we must
      --  read the file from disk, so we first write the embedded data to a
      --  temporary file we can read.

      declare
         Contents : constant AAA.Strings.Vector :=
                      AAA.Strings.To_Vector (As_String (Src));
         Tmpl     : Utils.Text_Files.File  :=
                      Utils.Text_Files.Create (Temp.Filename);
      begin
         Tmpl.Lines.all := Contents;
      end;

      --  At this point the source template exists on disk. We also take the
      --  opportunity to replace UNIX \n in the file so generation on other
      --  platforms uses the proper line endings.

      declare
         Parsed : constant String :=
                    Templates_Parser.Parse
                      (Filename     => Temp.Filename,
                       Translations => Map.Set,
                       Report       => Parsing_Error'Access);
         Content : constant AAA.Strings.Vector
           := AAA.Strings.Split (Parsed,
                                 Ada.Characters.Latin_1.LF,
                                 Trim => True);
         File : Utils.Text_Files.File := Utils.Text_Files.Create (Dst);
      begin
         File.Lines.all := Content;
      end;
   end Translate_File;

   --------------------
   -- Translate_Tree --
   --------------------

   procedure Translate_Tree (Parent : Relative_Path;
                             Files  : Tree'Class;
                             Map    : Translations)
   is
      package Dirs renames Directories;
      package TP renames Templates_Parser;
      use Directories.Operators;
      use File_Data_Maps;
   begin
      for I in Files.Iterate loop
         declare
            Raw_Name : constant Portable_Path := Key (I);

            --  Translate the path
            Entry_Name : constant Relative_File :=
                           VFS.To_Native
                             (Portable_Path
                                (TP.Translate (String (Raw_Name),
                                               Map.Set)));
         begin
            --  Ensure parent exists
            Dirs.Create_Tree (Parent / Dirs.Parent (Entry_Name));

            --  And translate the actual file or create the directory
            if Files (I).Is_Dir then
               Dirs.Create_Tree (Parent / Entry_Name);
            else
               declare
                  File_Name : constant Any_Path := Parent / Entry_Name;
               begin
                  if Den.Exists (File_Name) then
                     Raise_Checked_Error
                       ("Cannot generate, "
                        & TTY.URL (File_Name) & " already exists");
                  else
                     Translate_File (Files (I).Data,
                                     Parent / Entry_Name,
                                     Map);
                  end if;
               end;
            end if;
         end;
      end loop;
   end Translate_Tree;

end Alire.Templates;
