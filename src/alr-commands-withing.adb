with Ada.Directories;
with Alr.Hardcoded;
with Alr.OS_Lib;

package body Alr.Commands.Withing is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
   begin
      null;
   end Execute;

   --------------------
   -- Locate_Package --
   --------------------

   function Locate_Package (Name : Alire.Projects.Names) return String is

      function Verify (File : String) return Boolean is (True);
      --  FIXME: use libadaland or a tokenizer or whatever to ensure that
      --  a likely file indeed contains the declaration of a release for the
      --  given project

      use Ada.Directories;

      Strict : Boolean := True;
      --  First pass we look for the exact name,
      --  second pass we look for a prefix

      procedure Matches (File : Directory_Entry_Type) is
      begin
      end Matches;

   begin
      OS_Lib.Traverse_Folder (Hardcoded.Alr_Branch
   end Locate_Package;

end Alr.Commands.Withing;
