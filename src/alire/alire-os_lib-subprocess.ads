with Alire.Utils;

package Alire.OS_Lib.Subprocess is

   --  Separate from Alire.OS_Lib because not preelaborable

   function Locate_In_Path (Name : String) return String;
   --  Returns full path to Name command or "" if not found

   procedure Checked_Spawn
     (Command             : String;
      Arguments           : Utils.String_Vector;
      Understands_Verbose : Boolean := False);
   --  Either suceeds or raises Checked_Error with the code and output as info.

   function Checked_Spawn_And_Capture
     (Command             : String;
      Arguments           : Utils.String_Vector;
      Understands_Verbose : Boolean := False;
      Err_To_Out          : Boolean := False) return Utils.String_Vector;
   --  Either suceeds or raises Checked_Error with the code and output as info.
   --  Output is captured and returned.

end Alire.OS_Lib.Subprocess;
