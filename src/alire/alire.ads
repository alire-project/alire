with Ada.Assertions;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
private with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

pragma Warnings (Off, "no entities of * are referenced");
with AAA.Strings;
pragma Unreferenced (AAA.Strings);
--  AAA.Strings is used everywhere in Alire, so we make it with-visible for all
--  units.
pragma Warnings (On, "no entities of * are referenced");

pragma Warnings (Off);
with GNAT.OS_Lib;
pragma Warnings (On);

with Simple_Logging;
with CLIC.TTY;

package Alire with Preelaborate is

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

   Min_Tag_Length         : constant := 1;
   Max_Tag_Length         : constant := 15;
   --  Maximum length of a single element of the tags field

   --  package BStrings is new Ada.Strings.Bounded.Generic_Bounded_Length
   --       (Integer'Max (Max_Name_Length, Max_Description_Length));

   Extension_Separator    : constant Character := '.';
   --  Refers to extension releases! Nothing to do with files

   Community_Trusted_Sites : constant String :=
     "bitbucket.org"
     & " github.com"
     & " gitlab.com"
     & " savannah.gnu.org"
     & " savannah.nongnu.org"
     & " sf.net";
   --  Space separated list of hosts that are known to not be vulnerable to
   --  SHA-1 collision attacks, and therefore trusted for use on the community
   --  index. Also used as the default value for 'origins.git.trusted_sites'.

   --  Strings that are used quite generally

   package UStrings renames Ada.Strings.Unbounded;
   subtype UString is Ada.Strings.Unbounded.Unbounded_String;

   function "+" (S : String)  return UString
   renames UStrings.To_Unbounded_String;

   function "+" (S : UString) return String
   renames UStrings.To_String;

   subtype Crate_Character is Character
      with Static_Predicate => Crate_Character in
         'a' .. 'z' | '0' .. '9' | '_';

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

   function Index_Prefix (This : Crate_Name) return String
     with Post => Index_Prefix'Result'Length = 2;
   --  The two first letters in the crate name

   subtype Restricted_Name is String with Dynamic_Predicate =>
     Restricted_Name'Length >= Min_Name_Length and then
     Restricted_Name (Restricted_Name'First) /= '_' and then
     (for all C of Restricted_Name => C in Crate_Character);
   --  A type used to limit some things that are given names by the user
   --  (e.g., remote index names).

   function "+" (P : Crate_Name) return String;
   function "+" (P : String)  return Crate_Name;
   function To_Name (S : String) return Crate_Name renames "+";

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

   type Portable_Path is new String with
     Dynamic_Predicate => (for all Char of Portable_Path => Char /= '\');
   --  A portable path always uses forward slashes. For use in the current
   --  platform, it should be adapted first.

   --  To clarify constants/functions declared herein:

   function Check_Absolute_Path (Path : Any_Path) return Boolean;
   --  Returns True if the string Path represent an absolute path on the
   --  platform, False otherwise.

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
     with Dynamic_Predicate => Check_Absolute_Path (Absolute_Path)
       or else raise Ada.Assertions.Assertion_Error
       with "Path is not absolute: " & Absolute_Path;

   function Absolute_Path_Image (Path : Absolute_Path) return String;
   --  Needed for later instantiations

   subtype Optional_Absolute_Path is Any_Path
     with Dynamic_Predicate =>
       Optional_Absolute_Path = "" or else
       Check_Absolute_Path (Optional_Absolute_Path);

   subtype Unbounded_Absolute_Path is UString
     with Dynamic_Predicate =>
       +Unbounded_Absolute_Path = "" or else
       Check_Absolute_Path (+Unbounded_Absolute_Path);

   subtype Relative_File is Any_Path;
   --  Filenames with relative paths

   subtype Relative_Path is Any_Path;
   --  A relative path

   subtype Unbounded_Relative_Path is UString
     with Dynamic_Predicate =>
       +Unbounded_Relative_Path = "" or else
     not Check_Absolute_Path (+Unbounded_Relative_Path);

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

   procedure Recoverable_User_Error (Msg : String; Recover : Boolean := Force);
   --  A User_Error is an attempt to do something that we don't allow by
   --  default, but that could make sense if you know what are doing in dubious
   --  situations. When Recover, emit a warning and return normally. When not
   --  Recover call Raise_Checked_Error instead.

   procedure Recoverable_Program_Error   (Explanation : String := "");
   --  This, instead, is for situations that should never happen but that
   --  are easy to detect and allow continuing, so instead of raising a
   --  Program_Error deliberately, we give the same kind of feedback but
   --  without raising.

   ---------------
   --  LOGGING  --
   ---------------

   use all type Simple_Logging.Levels;

   package Trace renames Simple_Logging;

   package TTY renames CLIC.TTY;

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

   --  Feedback-oriented commands for one-line feedback with symbol prefix:

   procedure Put_Info (Text : String; Level : Trace.Levels := Info);
   --  Prepend Text with a blue "🛈", or "Note: " & if no color/tty.

   procedure Put_Warning (Text           : String;
                          Level          : Trace.Levels := Warning;
                          Disable_Setting : String := "");
   --  Prepend Text with a yellow "⚠", or "Warning: " if no color/tty. If
   --  Disable_setting /= "", append a line informing about how to disable
   --  this warning.

   procedure Put_Success (Text : String; Level : Trace.Levels := Info);
   --  Prepend Text with a green check mark, or "Success:" if no color/tty.

   procedure Put_Failure (Text : String; Level : Trace.Levels := Info);
   --  Prepend Text with a red "✗", or "Failed:" if no color/tty. Intended as
   --  the opposite of Put_Success when it makes sense to continue, albeit
   --  briefly, without emitting a final error with Raise_Checked_Error.

   function Log (Text : String; Level : Trace.Levels := Info) return String;
   --  A convenience to be able to log inside declarative blocks. Returns Text.

   function New_Line return String;
   --  Returns the proper \n sequence based on the platform

   ---------------
   -- Constants --
   ---------------

   GNAT_Crate          : constant Crate_Name;
   GNAT_External_Crate : constant Crate_Name;
   GNAT_Native_Crate   : constant Crate_Name;
   GPRbuild_Crate      : constant Crate_Name;

private

   type Crate_Name (Len : Natural) is tagged record
      Name : String (1 .. Len);
   end record;

   function Length (This : Crate_Name) return Positive is (This.Len);

   function As_String (This : Crate_Name) return String is (This.Name);

   function Index_Prefix (This : Crate_Name) return String
   is (This.Name (This.Name'First .. This.Name'First + 1));

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

   GNAT_Crate     : constant Crate_Name := (Len => 4, Name => "gnat");
   GPRbuild_Crate : constant Crate_Name := (Len => 8, Name => "gprbuild");

   GNAT_External_Crate : constant Crate_Name :=
                           (Len => 13, Name => "gnat_external");

   GNAT_Native_Crate : constant Crate_Name :=
                           (Len => 11, Name => "gnat_native");

   function U (S          : Wide_Wide_String;
               Output_BOM : Boolean := False)
               return Ada.Strings.UTF_Encoding.UTF_8_String
               renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode;

end Alire;
