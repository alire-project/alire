private with AAA.Enum_Tools;
with AAA.Strings;

with Alire.Directories;
with Alire.Roots.Optional;
with Alire.Solver;
with Alire.Version;

with CLIC.Subcommand;

private with GNAT.IO;
private with GNAT.Strings;
private with CLIC.Subcommand.Instance;

private with Alr.OS_Lib; -- For the benefit of many child packages that use it

package Alr.Commands is

   Wrong_Command_Arguments : exception;

   -------------
   -- Execute --
   -------------

   procedure Execute;
   --  Entry point into alr, will parse the command line and proceed as needed.

   -------------
   -- Command --
   -------------

   type Command
   is abstract limited new CLIC.Subcommand.Command
   with private;
   --  This type encapsulates configuration and execution of a specific
   --  command.

   overriding
   function Switch_Parsing (This : Command)
                            return CLIC.Subcommand.Switch_Parsing_Kind
   is (CLIC.Subcommand.Parse_All);
   --  Default for alr commands is to parse the switches

   -----------------------------------------
   -- Supporting subprograms for commands --
   -----------------------------------------

   function Root (Cmd : in out Command'Class)
                  return Alire.Roots.Optional.Reference;
   --  Using this call will ensure the Root detection has been attempted

   procedure Set (Cmd  : in out Command'Class;
                  Root : Alire.Roots.Root);
   --  Replace the current root in use by the command. Modifying the root via
   --  the Cmd.Root reference is valid and intended usage that does not require
   --  resetting the root.

   procedure Requires_Full_Index (Cmd          : in out Command'Class;
                                  Strict       : Boolean := False;
                                  Force_Reload : Boolean := False);
   --  Unless Force_Reload, if the index is not empty we no nothing. When
   --  strict, don't allow unknown values in enums.

   procedure Requires_Workspace (Cmd   : in out Command'Class;
                                 Sync  : Boolean := True;
                                 Error : String := "");
   --  Verifies that a valid workspace is in scope. After calling it,
   --  Cmd.Root will be usable if alr was run inside a Root. If Sync, enforce
   --  that the manifest, lockfile and dependencies on disk are in sync, by
   --  performing a silent update. If not Sync, only a minimal empty lockfile
   --  is created. If Error, replace the first generic error message with it.

   procedure Forbids_Structured_Output (Cmd : in out Command'Class);
   --  Use this to mark that the output of a command is not (yet) compatible
   --  with global flag --format.

   function Has_Root (Cmd : in out Command'Class) return Boolean;
   --  True when Requires_Workspace would succeed, false otherwise

   procedure Load (Cmd       : Command'Class;
                   Crate     : Alire.Crate_Name;
                   Externals : Boolean := False;
                   Strict    : Boolean := False);
   --  Load a specific crate from the index. Optionally detect externals and
   --  enforce no unknown enum index values.

   ---------------------------
   --  command-line helpers --
   ---------------------------

   function Is_Quiet return Boolean;
   --  Says if -q was in the command line

   function Query_Policy return Alire.Solver.Age_Policies;
   --  Current policy

   --  Declared here so they are available to the help metacommand child
   --  package and Spawn.

   function Crate_Version_Sets return AAA.Strings.Vector;
   --  Returns the instructions to restrict version sets, for use in
   --  Long_Description help functions.

   function Enter_Workspace_Root return Alire.Directories.Destination;
   --  Attempt to find the root alire workspace if deeper inside it

private

   type Command
   is abstract limited new CLIC.Subcommand.Command
     with record
      Optional_Root : Alire.Roots.Optional.Root;
   end record;

   --  Facilities for command/argument identification. These are available to
   --  commands.

   procedure Reportaise_Command_Failed  (Message : String) with No_Return;
   procedure Reportaise_Wrong_Arguments (Message : String) with No_Return;
   --  Report and Raise :P

   --  Folder guards conveniences for commands:

   subtype Folder_Guard is Alire.Directories.Guard;

   function Enter_Folder (Path : String) return Alire.Directories.Destination
   renames Alire.Directories.Enter;

   --  Common generalities

   procedure New_Line (Spacing : Positive := 1)
   renames GNAT.IO.New_Line;

   procedure Put_Line (S : String)
   renames GNAT.IO.Put_Line;

   procedure Put (S : String)
   renames GNAT.IO.Put;

   procedure Put_Error (Str : String);
   procedure Set_Global_Switches
     (Config : in out CLIC.Subcommand.Switches_Configuration);

   package Sub_Cmd is new CLIC.Subcommand.Instance
     (Main_Command_Name   => "alr",
      Version             => Alire.Version.Current.Image,
      Put                 => GNAT.IO.Put,
      Put_Line            => GNAT.IO.Put_Line,
      Put_Error           => Put_Error,
      Error_Exit          => OS_Lib.Bailout,
      Set_Global_Switches => Set_Global_Switches,
      TTY_Chapter         => Alire.TTY.Bold,
      TTY_Description     => Alire.TTY.Description,
      TTY_Version         => Alire.TTY.Version,
      TTY_Underline       => Alire.TTY.Underline,
      TTY_Emph            => Alire.TTY.Emph,
      Global_Options_In_Subcommand_Help => False);

   procedure Auto_Update_Index (This : Command);

   Unset : constant String := "unset";
   --  Canary for when a string switch is given without value

   subtype GNAT_String is GNAT.Strings.String_Access;
   --  Convenience for commands that use string arguments

   function Is_Boolean is new AAA.Enum_Tools.Is_Valid (Boolean);

   function To_Boolean (Image   : GNAT_String;
                        Switch  : String;
                        Default : Boolean)
                        return Boolean
     with Post =>
       (if Image in null or else Image.all = Unset
        then To_Boolean'Result = Default);
   --  Convert a switch value to a boolean, if explicitly given, or use the
   --  default otherwise. If not a valid boolean or empty, raise Checked_Error
   --  with an appropriate error message. NOTE: If the switch exists (is not
   --  unset) but has no argument, it's considered TRUE, not default.

end Alr.Commands;
