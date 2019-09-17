with Ada.Exceptions;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;

with Simple_Logging;

package Alire with Preelaborate is

   Checked_Error : exception;
   --  A Checked_Error is an explicitly diagnosed error condition, usually in
   --  relation with user inputs (e.g., parsing of TOML files or other inputs).
   --  Used internally in Alire in conjunction with Alire.Errors to use the
   --  normal exception mechanisms, that produce less boilerplate, while using
   --  Outcomes for results returned to clients. That is, a Checked_Error ought
   --  not to propagate into Alr.* code.

   Internal_Error : exception;
   --  While we transition to error codes, there are places that would require
   --  extensive refactoring. Also, legitimate irrecoverable situations may
   --  arise in places were an outcome cannot be easily returned. Instead of
   --  aborting and exiting from Alire, a last resort handler in Alr can catch
   --  this exception and exit gracefully. Should be used sparingly and of
   --  course not when user input is involved.

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

   overriding
   function "=" (L, R : Project) return Boolean;
   --  Project names are case preserving but insensitive when compared.

   overriding
   function "<" (L, R : Project) return Boolean;
   --  Likewise, we do not want capitalization to influence ordering.

   subtype Restricted_Name is String with Dynamic_Predicate =>
     Restricted_Name'Length >= Min_Name_Length and then
     Restricted_Name (Restricted_Name'First) /= '_' and then
     (for all C of Restricted_Name => C in Project_Character);
   --  A type used to limit some things that are given names by the user
   --  (e.g., remote index names).

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

   type Outcome is tagged private;
   --  For operations that may fail as part of normal usage.

   --  Status  --

   function Message (Result : Outcome) return String with
     Pre => not Result.Success;
   --  Returns some information in case of unsuccessful Outcome

   function Success (Result : Outcome) return Boolean;

   --  Constructors  --

   function Outcome_Failure (Message : String) return Outcome with
     Pre  => Message'Length > 0,
     Post => not Outcome_Failure'Result.Success;

   function Outcome_Success return Outcome with
     Post => Outcome_Success'Result.Success;

   function Outcome_From_Exception
     (Ex  : Ada.Exceptions.Exception_Occurrence;
      Msg : String := "") return Outcome with
     Post => not Outcome_From_Exception'Result.Success;
   --  Create a failed Outcome when a exception has occurred.
   --  The exception stack trace will be dumped at debug level.
   --  If message is empty, message will be Ex exception message.

   -----------------------
   -- Uncontained_Error --
   -----------------------

   procedure Uncontained_Error (Msg : String) with No_Return;
   --  For errors where we can't or won't (for now) proceed normally, nor
   --  return an Outcome_Failure, we trace an error message (Msg) and raise
   --  Internal_Error.

   ---------------
   --  LOGGING  --
   ---------------

   use all type Simple_Logging.Levels;

   package Trace renames Simple_Logging;

   Log_Level : Simple_Logging.Levels renames Simple_Logging.Level;
   --  This one selects the verbosity level of the logging library. The usage
   --  of log levels in Alire is as follows. By default, no output is produced
   --  unless in case of warning or error. The log levels supported are:
   --  * ERROR:   for fatal situations that preclude further operation.
   --  * Warning: for suspicious situations, but where alr continues normally.
   --             Hidden with '-q' switch.
   --  * Info:    messages that are shown by default. Output that is the result
   --             of user requests in normal situations.
   --  * Detail:  not shown by default, enabled with '-v' switch, that
   --             may be interesting to and intended for regular users.
   --  * Debug:   not shown by default, enabled with '-vv' switch, messages
   --             intended for developers or curious users, not user friendly.

   Log_Debug : aliased Boolean := False;
   --  This one enables special debug output, irrespectively of the log level.

   procedure Log_Exception (E     : Ada.Exceptions.Exception_Occurrence;
                            Level : Simple_Logging.Levels := Debug);

private

   type Outcome is tagged record
      Success : Boolean := False;
      Message : Ada.Strings.Unbounded.Unbounded_String :=
                  +"Uninitialized Outcome";
   end record;
   --  We cannot simultaneously be tagged and have default constraints; the
   --  small overhead of always having the Message member is the price to pay.

   function Message (Result : Outcome) return String is (+Result.Message);

   function Outcome_Failure (Message : String) return Outcome is
     (Success => False,
      Message => +Message);

   function Outcome_Success return Outcome is
     (Success => True,
      Message => +"");

   function Success (Result : Outcome) return Boolean is (Result.Success);

end Alire;
