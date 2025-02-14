with AAA.Strings;

package Alire.OS_Lib.Subprocess is

   --  Separate from Alire.OS_Lib because not preelaborable

   function Locate_In_Path (Name : String) return String;
   --  Returns full path to Name command or "" if not found

   procedure Checked_Spawn
     (Command             : String;
      Arguments           : AAA.Strings.Vector;
      Understands_Verbose : Boolean := False);
   --  Either succeeds or raises Checked_Error with the code and output as
   --  info.

   type Code_Array is array (Positive range <>) of Integer;
   --  An array of exit codes that won't cause the following calls to raise

   function Checked_Spawn_And_Capture
     (Command             : String;
      Arguments           : AAA.Strings.Vector;
      Understands_Verbose : Boolean := False;
      Err_To_Out          : Boolean := False;
      Valid_Exit_Codes    : Code_Array := (1 => 0)) return AAA.Strings.Vector;
   --  Either succeeds or raises Checked_Error with the code and output as
   --  info. Output is captured and returned on success. The exit code is
   --  checked against the Valid_Exit_Codes.

   function Unchecked_Spawn_And_Capture
     (Command             : String;
      Arguments           : AAA.Strings.Vector;
      Output              : in out AAA.Strings.Vector;
      Understands_Verbose : Boolean := False;
      Err_To_Out          : Boolean := False) return Integer;
   --  Returns the output and exit code of the spawned command

   function Unchecked_Spawn
     (Command             : String;
      Arguments           : AAA.Strings.Vector;
      Understands_Verbose : Boolean := False) return Integer;
   --  Doesn't capture output but doesn't fail on error either

end Alire.OS_Lib.Subprocess;
