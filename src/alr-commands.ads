with GNAT.Command_Line;

with Alr.Bootstrap;
with Alr.Query;

private with Ada.Text_IO;

private with Alire.GPR;
private with Alire.Projects; pragma Unreferenced (Alire.Projects);
private with Alire.Properties.Labeled;

private with Alr.OS_Lib;
pragma Warnings (Off); private with Alr.Root_Release; pragma Warnings (On);
private with Alr.Utils;

package Alr.Commands is

   Wrong_Command_Arguments : exception;

   -------------
   -- Execute --
   -------------

   procedure Execute;
   --  Entry point into alr, will parse the command line and proceed as needed

   -------------
   -- Command --
   -------------

   type Command is limited interface;
   --  This type encapsulates configuration and execution of a specific command

   procedure Display_Help_Details (Cmd : Command) is null;

   procedure Execute (Cmd : in out Command) is abstract;
   --  May raise Command_Failed

   procedure Setup_Switches (Cmd    : in out Command;
                             Config : in out GNAT.Command_Line.Command_Line_Configuration) is null;

   function Short_Description (Cmd : Command) return String is abstract;
   --  One-liner displayed in the main help after the command name

   function Usage_Custom_Parameters (Cmd : Command) return String is abstract;
   --  The part after "alr command "
   --  That is, the ones not managed by Gnat.Command_Line

   -----------------------------------------
   -- Supporting subprograms for commands --
   -----------------------------------------

   --  These are in order of exigence
   use all type Bootstrap.Session_States;

   procedure Requires_Full_Index;
   --  Ensures that alr has self-built with a full index

   procedure Requires_Project;
   --  Checks and performs session is up to date, and that the project matches to continue with it
   --  May trigger recompilation and respawn. In that case, it doesn't return to the caller, but respawns.
   --  If it returns, then we are running the updated alr executable for the current session+project

   procedure Requires_Buildfile;
   --  Ensures that the build file exists, and if not generates one from dependencies

   ---------------------------
   --  command-line helpers --
   ---------------------------

   function Global_Switches return String;
   --  This is exported only to be reachable from Spawn, but there's no reason to use it from commands
   --  Returns the in use global switches (-d -q -v)
   --  Useful e.g. to pass along on respawning a custom command

   function Is_Quiet return Boolean;
   --  Says if -q was in the command line

   function Query_Policy return Query.Policies;
   --  Current policy

   -- Declared here so they are available to the help metacommand child package and Spawn

   procedure Print_Project_Version_Sets;
   --  How to specify project version sets (to be used in specific command help)

   type Cmd_Names is (Cmd_Build,
                      Cmd_Clean,
                      Cmd_Compile,
                      Cmd_Dev,
                      Cmd_Get,
                      Cmd_Init,
                      Cmd_List,
                      Cmd_Pin,
                      Cmd_Run,
                      Cmd_Search,
                      Cmd_Test,
                      Cmd_Update,
                      Cmd_Version);
   --  The Cmd_ prefix allows the use of the proper name in child packages which otherwise cause conflict
   --  It is a bit ugly but also it makes clear when we are using this enumeration

   function Image (N : Cmd_Names) return String;

private

   --  Visibility for some enums

   use Alire.Projects;
   use all type Alire.Properties.Labeled.Labels;

   --  Session shortcut
   function Session_State return Bootstrap.Session_States renames Bootstrap.Session_State;

   --  Facilities for command/argument identification. These are available to commands.

   procedure Reportaise_Command_Failed  (Message : String);
   procedure Reportaise_Wrong_Arguments (Message : String);
   --  Report and Raise :P

   Raw_Arguments : Utils.String_Vector; -- Raw arguments, first one is the command

   function What_Command return String;
   function What_Command return Cmd_Names;
   function Num_Arguments return Natural; -- Actual arguments besides the command
   function Argument (I : Positive) return String; -- May raise if not existing

   Scenario : Alire.GPR.Scenario;
   --  This will be filled in during parsing of command line with any seen "-X" parameters

   --  Other options

   procedure Display_Usage (Cmd : Cmd_Names);

   procedure Display_Valid_Commands;

   procedure Execute_By_Name (Cmd : Cmd_Names);
   -- Execute a command with the externally given command line

   --  Folder guards conveniences for commands:

   subtype Folder_Guard is OS_Lib.Folder_Guard;

   function Enter_Folder (Path : String) return Folder_Guard renames OS_Lib.Enter_Folder;

   function Enter_Project_Folder return Folder_Guard;
   --  If we have a compiled-in project, attempt to find its root above us
   --  Does nothing if we don't have a project, or if the root is not found

   --  Common generalities

   procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1) renames Ada.Text_IO.New_Line;
   procedure Put_Line (S : String) renames Ada.Text_IO.Put_Line;

end Alr.Commands;
