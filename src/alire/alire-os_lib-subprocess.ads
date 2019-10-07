with Alire.Utils;

package Alire.OS_Lib.Subprocess is

   --  Separate from Alire.OS_Lib because not preelaborable

   function Locate_In_Path (Name : String) return String;
   --  Returns full path to Name command or "" if not found

   procedure Checked_Spawn (Command   : String;
                            Arguments : String := "");
   --  Either suceeds or raises Checked_Error with the code and output as info.

   procedure Raw_Spawn (Program    : String;
                        Arguments  : Utils.String_Vector;
                        Output     : out Utils.String_Vector;
                        Exit_Code  : out Integer;
                        Err_To_Out : Boolean := True);
   --  Spawn without autoprocessing of arguments, which we may need in cases
   --  where arguments are convolutedly quoted, breaking standard GNAT
   --  autoseparation of arguments.

   function Spawn (Command             : String;
                   Arguments           : String := "";
                   Understands_Verbose : Boolean := False;
                   Force_Quiet         : Boolean := False) return Integer;
   --  Returns 0 on success, or command exit code, or -1 if other errors.
   --  If Understands, an extra -v will be passed on Debug log levels.
   --  If Force_Quiet and not in Debug level, output will be entirely muted
   --  (stdout & stderr).

   function Spawn_And_Capture (Output     : in out Utils.String_Vector;
                               Command    : String;
                               Arguments  : String := "";
                               Err_To_Out : Boolean := False) return Integer;
   --  Returns output as vector of strings
   --  Even if exception raised, Output will be filled-in

   function Spawn_And_Redirect (Out_File   : String;
                                Command    : String;
                                Arguments  : String := "";
                                Err_To_Out : Boolean := False) return Integer;
   --  Redirects output to file
   --  Raises CHILD_FAILED if exit code /= 0

private

   function Spawn_With_Progress (Command   : String;
                                 Arguments : String) return Integer;
   --  Captures output and makes it one-liner prefixing it with a spinner

end Alire.OS_Lib.Subprocess;
