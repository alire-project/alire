with GNAT.Command_Line;

pragma Warnings (Off);
private with Alr.Project;
pragma Warnings (On);
--  With it here so it's available to all child command packages

package Alr.Commands is
   
   procedure Execute;
   --  Entry point into alr, will parse the command line and proceed as needed
   
   type Command is limited interface;
   
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
   
   procedure Ensure_Valid_Project;
   --  Checks and performs session is up to date, and that the project matches to continue with it
   --  May trigger recompilation and respawn. In that case, it doesn't return to the caller, but respawns.
   --  If it returns, then we are running the updated alr executable for the current session+project
   
private 
   
   -- Declared here so they are available to the help metacommand child package   
   
   type Cmd_Names is (Cmd_Build,
                      Cmd_Clean,
                      Cmd_Compile,
                      Cmd_Dev,
                      Cmd_Execute,
                      Cmd_Generate,
                      Cmd_Get,
                      Cmd_Help,
                      Cmd_Init,
                      Cmd_Lock,
                      Cmd_Run,
                      Cmd_Search,
                      Cmd_Update,
                      Cmd_Upgrade,
                      Cmd_Version);
   --  The Cmd_ prefix allows the use of the proper name in child packages which otherwise cause conflict
   --  It is a bit ugly but also it makes clear when we are using this enumeration
   
   function Image (N : Cmd_Names) return String;
   
   procedure Display_Usage (Cmd : Cmd_Names);
   
   procedure Display_Valid_Commands;
   
   procedure Execute_By_Name (Cmd : Cmd_Names);
   -- Execute a command with the externally given command line
   
   function Last_Argument return String;
   --  Returns the last command-line argument, unless...
   --  If it begins with "-" (it's a switch) or there aren't at least three arguments,
   --    raise Wrong_Command_Arguments
   
end Alr.Commands;
