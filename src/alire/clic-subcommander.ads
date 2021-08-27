with AAA.Strings;
with GNAT.Strings;
with GNAT.Command_Line;

private with Ada.Strings.Unbounded;
private with Ada.Containers.Vectors;

package CLIC.Subcommander is

   --  This root package defines the interface types to be used in
   --  the Subcommander. See CLIC.Subcommander.Instance to create
   --  the parser/executor.

   type Switches_Configuration is limited private;
   --  This is a wrapper around GNAT.Command_Line.Command_Line_Configuration
   --  to provide extra features such as duplicate switch detection and custom
   --  usage format. The "Define_Switch" procedure below work exactly like the
   --  GNAT.Command_Line ones.

   procedure Define_Switch
     (Config      : in out Switches_Configuration;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Argument    : String := "ARG");

   procedure Define_Switch
     (Config      : in out Switches_Configuration;
      Output      : access Boolean;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Value       : Boolean := True);

   procedure Define_Switch
     (Config      : in out Switches_Configuration;
      Output      : access Integer;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Initial     : Integer := 0;
      Default     : Integer := 1;
      Argument    : String := "ARG");

   procedure Define_Switch
     (Config      : in out Switches_Configuration;
      Output      : access GNAT.Strings.String_Access;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Argument    : String := "ARG");

   procedure Define_Switch
     (Config      : in out Switches_Configuration;
      Callback    : not null GNAT.Command_Line.Value_Callback;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Argument    : String := "ARG");

   subtype Identifier is String
     with Predicate => (for all C of Identifier
                        => C in 'a' .. 'z' | 'A' .. 'Z' |
                                '0' .. '9' | '-' | '.' | '_');
   -------------
   -- Command --
   -------------

   type Command is limited interface;
   --  This type encapsulates configuration and execution of a specific
   --  command. It also has help-related subprograms.

   type Command_Access is access all Command'Class;

   function Name (Cmd : Command) return Identifier
   is abstract;
   --  Commands must override this procedure to provide the sub-command name.
   --  This name is used to identify the sub-command in usage and command line.
   --  E.g. "my_app <name>" will exectute the <name> command.

   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is abstract;
   --  Commands must override this procedure to provide the command
   --  functionality.

   function Long_Description (Cmd : Command) return AAA.Strings.Vector
   is abstract;
   --  Return a detailed description of the command. Each string in the vector
   --  is a paragraph that will be reformatted into appropriate length lines.

   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out Switches_Configuration)
   is null;
   --  Gets called once the command has been identified, but before the call to
   --  Execute. Config must be set up with the switches used by the command.

   function Short_Description (Cmd : Command) return String
   is abstract;
   --  One-liner displayed in the list of commands that alr understands that
   --  gets shown when no command or unknown command is given. Also shown as
   --  SUMMARY in the help of a specific command.

   function Usage_Custom_Parameters (Cmd : Command) return String
   is abstract;
   --  The part after "<main> [global options] command [command options] " that
   --  gets shown in USAGE in the command help summary. That is, it is the
   --  specific command-line part that is not managed via Gnat.Command_Line

   -----------------
   --  Help_Topic --
   -----------------

   type Help_Topic is limited interface;
   --  This type encapsulate the content of an "help topic", i.e. a piece of
   --  documentation that can displayed from the command line.

   type Help_Topic_Access is access all Help_Topic'Class;

   function Name (This : Help_Topic) return Identifier
   is abstract;
   --  This name is used to identify the topic in usage and command line.
   --  E.g. "my_app help <name>" will display the content of the <name> topic.

   function Title (This : Help_Topic) return String
   is abstract;
   --  Descriptive title for the topic content. Unlike the Name, the Title can
   --  containt whitespaces.

   function Content (This : Help_Topic) return AAA.Strings.Vector
   is abstract;
   --  Return the content of the help topic. Each string in the vector is a
   --  paragraph that will be reformatted into appropriate length lines.

   -----------
   -- Utils --
   -----------

   function No_TTY (Str : String) return String
   is (Str);
   --  Use this function for the TTY_* generic parameters of
   --  CLIC.Subcommander.Instance if you don't want or need TTY formating.

private

   type Switch_Info is record
      Switch      : Ada.Strings.Unbounded.Unbounded_String;
      Long_Switch : Ada.Strings.Unbounded.Unbounded_String;
      Help        : Ada.Strings.Unbounded.Unbounded_String;
      Argument    : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   --  Used internaly to store informations about the switches

   package Switch_Info_Vectors
   is new Ada.Containers.Vectors (Natural, Switch_Info);

   procedure Add (Vect : in out Switch_Info_Vectors.Vector;
                  Switch, Long_Switch, Help, Argument : String);

   type Switches_Configuration is limited record
      GNAT_Cfg : GNAT.Command_Line.Command_Line_Configuration;
      --  Still use GNAT.Command_Line to do the actual parsing

      Info     : Switch_Info_Vectors.Vector;
   end record;

   function Verify_No_Duplicates (A, B : Switches_Configuration)
                                  return Boolean;
   --  Returns True if there are no duplicates.
   --  Check that no switch is given twice in Config. This is used to ensure
   --  that command switches are not stepping on global switches, which would
   --  lead to some undefined behavior. This manual check is necessary because
   --  the GNAT library does not perform it.

end CLIC.Subcommander;
