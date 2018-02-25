with Ada.Exceptions;

with Alr.Session;

package body Alr.Self is

   function Parent (Path : String) return String renames Ada.Directories.Containing_Directory;

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
   -- Src_Folder --
   ----------------

   function Src_Folder return String is
     (if Is_Rolling
      then Parent (Parent (OS.Own_Executable))
      else Canonical_Folder);

end Alr.Self;
