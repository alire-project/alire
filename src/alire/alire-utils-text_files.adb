with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

with AAA.Strings; use AAA.Strings;

with Alire.Directories;

with LML;

package body Alire.Utils.Text_Files is

   ------------------
   -- Append_Lines --
   ------------------

   procedure Append_Lines (File       : Any_Path;
                           Lines      : AAA.Strings.Vector;
                           Backup     : Boolean  := True;
                           Backup_Dir : Any_Path := "")
   is
      F : Text_Files.File := Load (File, Backup, Backup_Dir);
   begin
      F.Lines.Append (Lines);
   end Append_Lines;

   -------------------
   -- Replace_Lines --
   -------------------

   procedure Replace_Lines (File       : Any_Path;
                            Lines      : AAA.Strings.Vector;
                            Backup     : Boolean  := True;
                            Backup_Dir : Any_Path := "")
   is
      F : Text_Files.File := Load (File, Backup, Backup_Dir);
   begin
      F.Lines := Lines;
   end Replace_Lines;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (This : in out File) is
      File : File_Type;
   begin
      if This.Lines = This.Orig then
         Trace.Debug ("No changes to save in " & This.Name);
         return;
      else
         Trace.Debug ("Replacing contents of " & This.Name);
      end if;

      declare
         Replacer : Directories.Replacer :=
                      Directories.New_Replacement (This.Name,
                                                   This.Backup,
                                                   This.Backup_Dir);
      begin
         Create (File, Out_File, Replacer.Editable_Name);
         for Line of This.Lines loop
            Put_Line (File, LML.Decode (Line));
         end loop;
         Close (File);
         Replacer.Replace;
      exception
         when E : others =>
            Log_Exception (E);
            raise;
      end;

   exception
      when E : others =>
         Alire.Utils.Finalize_Exception (E);
   end Finalize;

   -----------
   -- Lines --
   -----------

   function Lines (This : aliased in out File)
                   return access AAA.Strings.Vector
   is (This.Lines'Access);

   -----------
   -- Lines --
   -----------

   function Lines (Filename : Any_Path)
                   return AAA.Strings.Vector
   is
      F : constant File := Load (Filename);
   begin
      return F.Lines;
   end Lines;

   ------------
   -- Create --
   ------------

   function Create (Name : Any_Path) return File
   is (Ada.Finalization.Limited_Controlled with
       Length     => Name'Length,
       Backup_Len => 0,
       Name       => Name,
       Backup     => False,
       Backup_Dir => "",
       Lines      => <>,
       Orig       => <>);

   ----------
   -- Load --
   ----------

   function Load (From       : Any_Path;
                  Backup     : Boolean := True;
                  Backup_Dir : Any_Path := "")
                  return File
   is
      F : File_Type;
   begin
      return This : File := (Ada.Finalization.Limited_Controlled with
                             Length     => From'Length,
                             Backup_Len => Backup_Dir'Length,
                             Name       => From,
                             Backup     => Backup,
                             Backup_Dir => Backup_Dir,
                             Lines      => <>,
                             Orig       => <>)
      do
         Open (F, In_File, From);
         while not End_Of_File (F) loop
            This.Orig.Append (LML.Encode (Get_Line (F)));
         end loop;
         Close (F);

         This.Lines := This.Orig;
      end return;
   end Load;

end Alire.Utils.Text_Files;
