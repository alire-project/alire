generic

   Main_Command_Name : String;
   Version           : String;

   with procedure Set_Global_Switches
     (Config : in out GNAT.Command_Line.Command_Line_Configuration);

   with procedure Put (Str : String);
   with procedure Put_Line (Str : String);
   with procedure Put_Error (Str : String);
   with procedure Error_Exit (Code : Integer);

   with function TTY_Chapter (Str : String) return String;
   with function TTY_Description (Str : String) return String;
   with function TTY_Version (Str : String) return String;
   with function TTY_Underline (Str : String) return String;
   with function TTY_Emph (Str : String) return String;

package SubCommander.Instance is

   --  Instantiate this package to create a sub-command parser/executor

   procedure Register (Cmd : not null Command_Access);
   --  Register a sub-command

   procedure Register (Group : String; Cmd : not null Command_Access);
   --  Register a sub-command in a group

   procedure Register (Topic : not null Help_Topic_Access);
   --  Register an help topic

   function Parsed return Boolean;

   procedure Parse_Command_Line
     with Post => Parsed;
   --  Upon return, global switches have been processed so you can already
   --  change the behavior of your program based on these (e.g. verbosity).

   function What_Command return String
     with Pre => Parsed;

   procedure Execute
     with Pre => Parsed;
   --  Execute a command or display help/usage depending on command line args
   --  parsed with Parse_Command_Line.

   procedure Display_Usage (Displayed_Error : Boolean := False);
   procedure Display_Help (Keyword : String);

   Wrong_Command_Arguments : exception;
   Error_No_Command : exception;
   Command_Already_Defined : exception;

   type Builtin_Help is new Command with private;
   --  Use Register (new Builtin_Help); to provide a build-in help command

private

   -- Built-in Help Command --
   type Builtin_Help is new Command with null record;

   overriding
   function Name (This : Builtin_Help) return String
   is ("help");

   overriding
   procedure Execute (This : in out Builtin_Help;
                      Args :        AAA.Strings.Vector);

   overriding
   function Long_Description (This : Builtin_Help)
                              return AAA.Strings.Vector;

   overriding
   procedure Setup_Switches
     (This    : in out Builtin_Help;
      Config  : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding
   function Short_Description (This : Builtin_Help) return String;

   overriding
   function Usage_Custom_Parameters (This : Builtin_Help) return String;

end SubCommander.Instance;
