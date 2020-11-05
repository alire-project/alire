with Ada.Exceptions;
with Ada.Strings.Unbounded;

pragma Warnings (Off);
with GNAT.OS_Lib;
pragma Warnings (On);

with Simple_Logging;

package Alire with Preelaborate is

   Version : constant String := "0.7.1";

   Checked_Error : exception;
   --  A Checked_Error is an explicitly diagnosed error condition, usually in
   --  relation with user inputs (e.g., parsing of TOML files or other inputs).
   --  Used internally in Alire in conjunction with Alire.Errors to use the
   --  normal exception mechanisms, that produce less boilerplate, while using
   --  Outcomes for results returned to clients.

   Query_Unsuccessful : exception;
   --  Raised by subprograms that return releases/dependencies when not
   --  found/impossible.

   Unimplemented : exception;
   --  Features that are known to be missing and scheduled for near future
   --  implementation.

   subtype URL is String;

   Min_Name_Length        : constant := 3;
   Max_Name_Length        : constant := 64;
   --  Github maximum is 100 and bitbucket 128, cargo is 64 and npm 50...
   Max_Description_Length : constant := 72;
   --  Git line recommendation (although it's 50 for subject line)

   Max_Tag_Length         : constant := 15;
   --  Maximum length of a single element of the tags field

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

   subtype Crate_Character is Character
      with Static_Predicate => Crate_Character in
         'a' .. 'z' | '0' .. '9' | '_' | Extension_Separator;

   --------------------
   --  Crate Naming  --
   --------------------

   function Is_Valid_Name (S : String) return Boolean;
   function Error_In_Name (S : String) return String;
   --  Returns the problem with the crate name

   type Crate_Name (<>) is tagged private;

   overriding
   function "=" (L, R : Crate_Name) return Boolean;
   --  Crate names are case preserving but insensitive when compared.

   function "<" (L, R : Crate_Name) return Boolean;
   --  Likewise, we do not want capitalization to influence ordering.

   function Length (This : Crate_Name) return Positive;

   function As_String (This : Crate_Name) return String;

   function TTY_Image (This : Crate_Name) return String;

   subtype Restricted_Name is String with Dynamic_Predicate =>
     Restricted_Name'Length >= Min_Name_Length and then
     Restricted_Name (Restricted_Name'First) /= '_' and then
     (for all C of Restricted_Name => C in Crate_Character);
   --  A type used to limit some things that are given names by the user
   --  (e.g., remote index names).

   function "+" (P : Crate_Name) return String;
   function "+" (P : String)  return Crate_Name;

   subtype Description_String is String with Dynamic_Predicate =>
     Description_String'Length <= Max_Description_Length;

   subtype Folder_String is String
     with
       Dynamic_Predicate =>
         Folder_String'Length > 0 and then
         (for all C of Folder_String => C in
          'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | Extension_Separator);
   --  Used for cross-platform folder names

   subtype Any_Path is String;
   --  Base type for paths in Alire. These paths are always platform-dependent
   --  and can be used directly with filesystem functions.

   subtype Portable_Path is String with
     Dynamic_Predicate => (for all Char of Portable_Path => Char /= '\');
   --  A portable path always uses forward slashes. For use in the current
   --  platform, it should be adapted first.

   --  To clarify constants/functions declared herein:

   function Check_Absolute_Path (Path : Any_Path) return Boolean;
   --  Return True if the string Path represent an absolute path on the
   --  platform.

   subtype Directory_Path is Any_Path;

   subtype File_Path is Any_Path
     with Dynamic_Predicate =>
       File_Path (File_Path'Last) /= GNAT.OS_Lib.Directory_Separator;

   subtype Absolute_File is Any_Path
     with Dynamic_Predicate =>
       Check_Absolute_Path (Absolute_File)
        and then
       Absolute_File (Absolute_File'Last) /= GNAT.OS_Lib.Directory_Separator;
   --  Filenames with full path

   subtype Absolute_Path is Any_Path
     with Dynamic_Predicate => Check_Absolute_Path (Absolute_Path);

   subtype Relative_File is Any_Path;
   --  Filenames with relative paths

   subtype Relative_Path is Any_Path;
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

   function Outcome_Failure (Message : String;
                             Report  : Boolean := True)
                             return Outcome with
     Pre  => Message'Length > 0,
     Post => not Outcome_Failure'Result.Success;
   --  Calling this function generates a debug stack trace log, unless Report
   --  is set to False. For failures that are part of regular operation,
   --  this is recommended to avoid "scares" in the debug output.

   function Outcome_Success return Outcome with
     Post => Outcome_Success'Result.Success;

   function Outcome_From_Exception
     (Ex  : Ada.Exceptions.Exception_Occurrence;
      Msg : String := "") return Outcome with
     Post => not Outcome_From_Exception'Result.Success;
   --  Create a failed Outcome when a exception has occurred.
   --  The exception stack trace will be dumped at debug level.
   --  If message is empty, message will be Ex exception message.

   ----------------------
   -- Error generation --
   ----------------------

   Force : aliased Boolean := False;
   --  When True, recoverable errors are demoted to warnings and we keep going

   procedure Assert (Result : Outcome'Class);
   --  Does nothing for successful outcomes. Raises Checked_Error with the
   --  corresponding message set in Alire.Errors otherwise.

   procedure Assert (Condition : Boolean; Or_Else : String);
   --  Calls Raise_Checked_Error (Or_Else) when Condition is false

   procedure Raise_Checked_Error (Msg : String) with No_Return;
   --  For errors where we do not return an Outcome_Failure, we log an error
   --  message (Msg) and raise Checked_Error. There is no limitation on the
   --  length of Msg.

   procedure Recoverable_Error (Msg : String; Recover : Boolean := Force);
   --  When Recover, emit a warning and return normally. When not Recover call
   --  Raise_Checked_Error instead.

   ---------------
   --  LOGGING  --
   ---------------

   use all type Simple_Logging.Levels;

   package Trace renames Simple_Logging;

   Is_TTY : Boolean renames Simple_Logging.Is_TTY;
   --  Flag to enable ASCII control sequences for progress indicators. When
   --  redirecting the output these do not work and are too noisy. Defaults
   --  to False.

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

   function Detailed return Boolean;
   --  True when Log_Level is Detail or Debug

   Log_Debug : aliased Boolean := False;
   --  This one enables special debug output, irrespectively of the log level.

   procedure Log_Exception (E     : Ada.Exceptions.Exception_Occurrence;
                            Level : Simple_Logging.Levels := Debug);

   procedure Log_Info (Text : String; Level : Trace.Levels := Info);
   --  Prepend Text with a blue "🛈", or "Note: " & if no color/tty.

   procedure Log_Success (Text : String; Level : Trace.Levels := Info);
   --  Prepend Text with a green check mark, or "Success:" if no color/tty.

private

   type Crate_Name (Len : Natural) is tagged record
      Name : String (1 .. Len);
   end record;

   function Length (This : Crate_Name) return Positive is (This.Len);

   function As_String (This : Crate_Name) return String is (This.Name);

   function "+" (P : Crate_Name) return String is (P.Name);
   function "+" (P : String) return Crate_Name
   is (if Is_Valid_Name (P)
       then (P'Length, P)
       else raise Checked_Error with Error_In_Name (P));

   type Outcome is tagged record
      Success : Boolean := False;
      Message : Ada.Strings.Unbounded.Unbounded_String :=
                  +"Uninitialized Outcome";
   end record;
   --  We cannot simultaneously be tagged and have default constraints; the
   --  small overhead of always having the Message member is the price to pay.

   function Message (Result : Outcome) return String is (+Result.Message);

   function Outcome_Success return Outcome is
     (Success => True,
      Message => +"");

   function Success (Result : Outcome) return Boolean is (Result.Success);

   function Detailed return Boolean is
     (Log_Level >= Detail);

end Alire;
