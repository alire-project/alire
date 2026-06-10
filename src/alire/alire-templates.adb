with Ada.Characters.Latin_1;
with Ada.Directories;
with Alire.Directories;
with Alire.Errors;
with Alire.Utils.Text_Files;
with Alire.VFS;

with Den;

package body Alire.Templates is

   ---------------------
   -- Add_Translation --
   ---------------------

   procedure Add_Translation (Map : in out Translations;
                              Var : String;
                              Val : String)
   is
      use Templates_Parser;
   begin
      Insert (Map.Set, Assoc (Var, Val));
   end Add_Translation;

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

   ----------------------
   -- Translate_String --
   ----------------------

   function Translate_String (Template : String;
                              Map : Translations)
                              return String
   is
      Temp : Directories.Temp_File; -- temporary template on disk
   begin
      --  To gain access to the full capabilities of Templates_Parser we must
      --  read the file from disk, so we first write the string data to a
      --  temporary file we can read.

      declare
         Contents : constant AAA.Strings.Vector :=
                      AAA.Strings.To_Vector (Template);
         Tmpl     : Utils.Text_Files.File  :=
                      Utils.Text_Files.Create (Temp.Filename);
      begin
         Tmpl.Lines.all := Contents;
      end;

      declare
         Parsed : constant String :=
           Templates_Parser.Parse
             (Filename     => Temp.Filename,
              Translations => Map.Set,
              Report       => Parsing_Error'Access);
      begin
         return Parsed;
      end;
   end Translate_String;

   --------------------
   -- Translate_File --
   --------------------

   procedure Translate_File (Src : Any_Path;
                             Dst : Relative_File;
                             Map : Translations)
   is
   begin
      --  We also take the opportunity to replace UNIX \n in the file so
      --  generation on other platforms uses the proper line endings.

      declare
         Parsed : constant String :=
                    Templates_Parser.Parse
                      (Filename     => Src,
                       Translations => Map.Set,
                       Report       => Parsing_Error'Access);
         Content : AAA.Strings.Vector
           := AAA.Strings.Split (Parsed,
                                 Ada.Characters.Latin_1.LF);
         File : Utils.Text_Files.File := Utils.Text_Files.Create (Dst);
      begin
         --  Remove a possibly empty last line
         if not Content.Is_Empty and then Content.Last_Element = "" then
            Content.Delete_Last;
         end if;

         File.Lines.all := Content;
      end;
   end Translate_File;

   --------------------
   -- Translate_File --
   --------------------

   procedure Translate_File (Src : Embedded;
                             Dst : Relative_File;
                             Map : Translations)
   is
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

      Translate_File (Temp.Filename, Dst, Map);
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

   --------------------
   -- Translate_Tree --
   --------------------

   procedure Translate_Tree (Root : Absolute_Path;
                             Map  : Translations)
   is
      Suffix : constant String := "_tmplt";

      To_Translate : AAA.Strings.Vector := AAA.Strings.Empty_Vector;

      procedure Register_Item_To_Translate (Item : Any_Path;
                                            Stop : in out Boolean)
      is
         Exclude_Alire : constant Absolute_Path := Root & "/alire";
         Exclude_Git : constant Absolute_Path := Root & "/.git";
      begin
         if Item = Exclude_Alire or else Item = Exclude_Git then
            raise Alire.Directories.Traverse_Tree_Prune_Dir;
         end if;
         if AAA.Strings.Has_Suffix (Item, Suffix) then
            To_Translate.Append (Item);
         end if;
         Stop := False;
      end Register_Item_To_Translate;

      function Longer (A, B : String) return Boolean
      is (A'Length > B'Length);

      package Sort_By_Len
      is new AAA.Strings.Vectors.Generic_Sorting (Longer);

      procedure Translate_Item (Item : Absolute_Path) is
         pragma Assert (AAA.Strings.Has_Suffix (Item, Suffix));

         Simple : constant Any_Path := Ada.Directories.Simple_Name (Item);
         Dir    : constant Absolute_Path :=
           Ada.Directories.Containing_Directory (Item);

         No_Suffix : constant Any_Path :=
           Simple (Simple'First .. Simple'Last - Suffix'Length);

         New_Name : constant Any_Path := Translate_String (No_Suffix, Map);
         --  Apply translations to the file/dir name

         New_Full_Name : constant Absolute_Path := Dir & "/" & New_Name;
         --  Absolute path of the new file/dir after tranlsation

      begin
         Trace.Debug ("Templates: translation " & Item & " into " &
                        New_Full_Name);

         if Alire.Directories.Is_Directory (Item) then
            Alire.Directories.Rename (Item, New_Full_Name);
            Trace.Debug ("Templates: rename dir to -> " & New_Full_Name);
         elsif Alire.Directories.Is_File (Item) then
            Trace.Always ("Templates: translate file into -> " &
                            New_Full_Name);
            Translate_File (Item, New_Full_Name, Map);
            Alire.Directories.Force_Delete (Item);
         end if;

      end Translate_Item;

   begin

      --  List items to translate
      Alire.Directories.Traverse_Tree (Root,
                                       Register_Item_To_Translate'Access,
                                       Recurse => True,
                                       Spinner => True);

      --  We sort the items to translate by length, longer first. This way we
      --  can translate the items deeper in the directory structure first.
      Sort_By_Len.Sort (AAA.Strings.Vectors.Vector (To_Translate));

      for Elt of To_Translate loop
         Translate_Item (Elt);
      end loop;

   end Translate_Tree;

end Alire.Templates;
