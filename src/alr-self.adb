with Ada.Exceptions;

with Alr.Session;
with Alr.Utils;

package body Alr.Self is

   function Parent (Path : String) return String renames Ada.Directories.Containing_Directory;

   --------------------
   -- Has_Full_Index --
   --------------------

   function Has_Full_Index return Boolean is (Session.Full_Index);

   ------------------
   -- Is_Bootstrap --
   ------------------

   function Is_Bootstrap return Boolean is
   begin
      pragma Warnings (Off);
      return Session.Hash = Bootstrap_Hash;
      pragma Warnings (On);
   end Is_Bootstrap;

   ------------------
   -- Is_Canonical --
   ------------------

   function Is_Canonical return Boolean is
      (Is_Rolling and then Src_Folder = OS.Config_Folder / "alr");
   --  Master check that says if we are running a canonically-built alr, or some other

   ----------------
   -- Is_Rolling --
   ----------------

   function Is_Rolling return Boolean is
      use Ada.Directories;
   begin
      return
        Simple_Name (Parent (OS.Own_Executable)) = "bin" and then
        Exists (Parent (Parent (OS.Own_Executable)) / "alr_env.gpr");
   exception
      when E : others =>
         Trace.Debug ("Exception while trying to detect alr src folder: " &
                        Ada.Exceptions.Exception_Name (E) &
                        " with msg: " & Ada.Exceptions.Exception_Message (E));
         return False;
   end Is_Rolling;

   ----------------
   -- Is_Session --
   ----------------

   function Is_Session return Boolean is (Session.Session_Build);

   ---------------------
   -- Matches_Session --
   ---------------------

   function Matches_Session (Metafile : String) return Boolean is
      (Session.Hash = Utils.Hash_File (Metafile));

   ----------------
   -- Src_Folder --
   ----------------

   function Src_Folder return String is
   begin
      if Session.Alr_Src_Folder /= "" then
         return Session.Alr_Src_Folder;
      elsif Is_Rolling Then
         return Parent (Parent (OS.Own_Executable));
      else
         return Canonical_Folder;
      end if;
   end Src_Folder;

end Alr.Self;
