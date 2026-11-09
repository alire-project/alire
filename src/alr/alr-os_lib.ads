with Ada.Directories;

with Alire.Directories;

with GNAT.OS_Lib;

package Alr.OS_Lib is

   Line_Separator : constant String;

   --  Environment

   function Getenv (Var : String; Default : String := "") return String;

   procedure Setenv (Var : String; Value : String) renames GNAT.OS_Lib.Setenv;

   function Exists_In_Path (File : String) return Boolean is
     (GNAT.OS_Lib."/="  (GNAT.OS_Lib.Locate_Exec_On_Path (File), null));
   --  FIXME: memory leak in this call

   --  OS PORTABLE FUNCTIONS

   Pause_Before_Exit : Boolean := False with Atomic;
   --  When set, Bailout waits for the user to press Enter before exiting, if
   --  interactive. Used by the Windows self-update helper process, whose
   --  console window would otherwise close before its errors can be read.

   procedure Bailout (Code : Integer := 0);
   --  Exit immediately with the given code (see Alire.OS_Lib.Bailout), after
   --  pausing if Pause_Before_Exit is set.

   function Is_Older (This : String; Than : String) return Boolean;
   --  Says if This file is older than Than

   --  GENERAL COMMAND LINE

   function Current_Folder return String
   renames Ada.Directories.Current_Directory;

   --  PATH BUILDING

   function "/" (L, R : String) return String;
   --  Shorthand for path composition

   package Paths is
      function "/" (L, R : String) return String renames Alr.OS_Lib."/";
   end Paths;

   -----------------------------
   --  CURRENT FOLDER MANAGEMENT

   subtype Folder_Guard is Alire.Directories.Guard;

   function Enter_Folder (Path : String) return Alire.Directories.Destination
                          renames Alire.Directories.Enter;

   function Stay_In_Current_Folder return Alire.Directories.Destination
     renames Alire.Directories.Stay_In_Current;

   ----------------------------
   --  FILE / FOLDER MANAGEMENT

   function Is_Executable_File (Path : String) return Boolean
   renames GNAT.OS_Lib.Is_Executable_File;

   function Is_Regular_File    (Path : String) return Boolean
   renames GNAT.OS_Lib.Is_Regular_File;

   function Is_Folder (Path : Alire.Any_Path) return Boolean
   renames GNAT.OS_Lib.Is_Directory;

private

   Line_Separator : constant String := Latin_1.LF & "";
   --  This should be made OS independent

   function "/" (L, R : String) return String is
     (L & GNAT.OS_Lib.Directory_Separator & R);

end Alr.OS_Lib;
