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

   function Spawn (Command             : String;
                   Arguments           : Utils.String_Vector;
                   Understands_Verbose : Boolean := False;
                   Force_Quiet         : Boolean := False) return Integer;
   --  Returns 0 on success, or command exit code, or -1 if other errors.
   --  If Understands, an extra -v will be passed on Debug log levels.
   --  If Force_Quiet and not in Debug level, output will be entirely muted
   --  (stdout & stderr).

   function Spawn_And_Capture
     (Output              : in out Utils.String_Vector;
      Command             : String;
      Arguments           : Utils.String_Vector;
      Understands_Verbose : Boolean := False;
      Err_To_Out          : Boolean := False)
      return Integer;
   --  Returns output as vector of strings
   --  Even if exception raised, Output will be filled-in

private

   function Spawn_With_Progress
     (Command   : String;
      Arguments : Utils.String_Vector)
      return Integer;
   --  Captures output and makes it one-liner prefixing it with a spinner

end Alire.OS_Lib.Subprocess;
