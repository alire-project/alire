with Ada.Directories;

with Alire.Utils;

private with Ada.Finalization;

package Alire.Directories is

   function "/" (L, R : String) return String
   is (Ada.Directories.Compose (L, R));

   -- Package to enable easy use of "/"
   package Operators is
      function "/" (L, R : String) return String renames Directories."/";
   end Operators;

   procedure Create_Directory (Name : Platform_Independent_Path);
   --  Creates a directory and all necessary parent ones.
   --  May raise usual filesystem exceptions.

   function Current return String renames Ada.Directories.Current_Directory;

   function Parent (Dir : String) return String
                    renames Ada.Directories.Containing_Directory;

   function Detect_Root_Path (Starting_At : Absolute_Path := Current)
                              return String;
   --  Return either the valid enclosing root folder, or ""

   function Find_Files_Under (Folder    : String;
                              Name      : String;
                              Max_Depth : Natural := Natural'Last)
                              return Utils.String_Vector;
   --  Recursively search for a file
   --  Depth 0 means given folder only
   --  Returns all instances found

   function Find_Single_File (Path      : String;
                              Extension : String)
                              return Absolute_Path;
   --  Finds a single file in a folder with the given extension.
   --  If more than one, or none, returns ""

   ----------------
   -- GUARD TYPE --
   ----------------
   --  This type simplifies staying in a folder during the life of a scope.
   --  Once the scope ends, the current folder is set back to the one it was.

   type Destination is access String;
   Stay : constant Destination;

   type Guard (Enter : Destination := Stay) is limited private;
   --  use this type in conjunction with Enter_Folder to ensure that the CWD is
   --  modified and restored when creating/destroying the Folder_Guard.

   function Enter (Path : String) return Destination is (new String'(Path));

   function Stay_In_Current return Destination is (Stay);
   --  This whole mess of accesses and leaks is due to a bug in the
   --    in-place initialization of limited

private

   Stay : constant Destination := null;

   type Guard (Enter : Destination := Stay)
   is new Ada.Finalization.Limited_Controlled
   with record
      Original : UString;
   end record;

   overriding procedure Initialize (This : in out Guard);
   overriding procedure Finalize   (This : in out Guard);

end Alire.Directories;
