with Ada.Exceptions;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;

with Simple_Logging;

package Alire with Preelaborate is

   Query_Unsuccessful : exception;
   --  Raised by subprograms that return releases/dependencies when not
   --  found/impossible.

   Unimplemented      : exception;
   --  Features that are known to be missing and scheduled for near future
   --  implementation.

   subtype URL is String;

   Min_Name_Length        : constant := 3;
   Max_Name_Length        : constant := 72;
   --  Github maximum is 100 and bitbucket 128, but since Description is 72...
   Max_Description_Length : constant := 72;
   --  Git line recommendation (although it's 50 for subject line)

   --  package BStrings is new Ada.Strings.Bounded.Generic_Bounded_Length
   --       (Integer'Max (Max_Name_Length, Max_Description_Length));

   Extension_Separator    : constant Character := '.';
   --  Refers to extension releases! Nothing to do with files

   --  Strings that are used quite generally

   package UStrings renames Ada.Strings.Unbounded;
   subtype UString is Ada.Strings.Unbounded.Unbounded_String;

   function "+" (S : String)  return UString
   renames UStrings.To_Unbounded_String;

   function "+" (S : UString) return String
   renames UStrings.To_String;

   subtype Project_Character is Character
      with Static_Predicate => Project_Character in
         'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | Extension_Separator;

   type Project is new String with Dynamic_Predicate =>
     Project'Length >= Min_Name_Length and then
     Project (Project'First) /= '_' and then
     Project (Project'First) /= Extension_Separator and then
     Project (Project'Last) /= Extension_Separator and then
     (for all C of Project => C in Project_Character);

   function "+" (P : Project) return String  is (String (P));
   function "+" (P : String)  return Project is (Project (P));

   subtype Description_String is String with Dynamic_Predicate =>
     Description_String'Length <= Max_Description_Length;

   subtype Folder_String is String
     with
       Dynamic_Predicate =>
         Folder_String'Length > 0 and then
         (for all C of Folder_String => C in
          'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | Extension_Separator);
   --  Used for cross-platform folder names

   subtype Platform_Independent_Path is String
     with
       Dynamic_Predicate =>
         (for all C of Platform_Independent_Path => C /= '\');
   --  This type is used to ensure that folder separators are externally always
   --  '/', and internally properly converted to the platform one

   --  To clarify constants/functions declared herein:

   subtype Absolute_File is Platform_Independent_Path
     with
       Dynamic_Predicate =>
         Absolute_File (Absolute_File'First) = GNAT.OS_Lib.Directory_Separator;
   --  Filenames with full path

   subtype Absolute_Path is Platform_Independent_Path
     with
       Dynamic_Predicate =>
         Absolute_Path (Absolute_Path'First) = GNAT.OS_Lib.Directory_Separator;

   subtype Relative_File is Platform_Independent_Path;
   --  Filenames with relative paths

   subtype Relative_Path is Platform_Independent_Path;
   --  A relative path

   subtype Simple_File is String
     with
       Dynamic_Predicate =>
         (for all C of Simple_File => C /= GNAT.OS_Lib.Directory_Separator);
   --  Filenames without path

   ----------------
   --  Outcomes  --
   ----------------

   --  For operations that may fail as part of normal usage.

   type Outcome (Success : Boolean := True) is record
      case Success is
         when True  => null;
         when False => Message : Ada.Strings.Unbounded.Unbounded_String;
      end case;
   end record;

   function Message (Result : Outcome) return String is (+Result.Message);

   function Outcome_Failure (Message : String) return Outcome is
      (Success => False, Message => +Message);

   function Outcome_Success return Outcome is (Success => True);

   function Outcome_From_Exception
     (Ex  : Ada.Exceptions.Exception_Occurrence;
      Msg : String := "") return Outcome with
     Post => not Outcome_From_Exception'Result.Success;
   --  Create a failed Outcome when a exception has occurred.
   --  The exception stack trace will be dumped at debug level.
   --  If message is empty, message will be Ex exception message.

   ---------------
   --  LOGGING  --
   ---------------

   use all type Simple_Logging.Levels;

   package Trace renames Simple_Logging;

   Log_Level : Simple_Logging.Levels renames Simple_Logging.Level;

   procedure Log (S : String; Level : Simple_Logging.Levels := Info)
                  renames Simple_Logging.Log;

   procedure Log_Exception (E     : Ada.Exceptions.Exception_Occurrence;
                            Level : Simple_Logging.Levels := Debug);

end Alire;
