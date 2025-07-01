with AAA.Strings;

with Alire.Formatting;
with Alire.Releases;

package Alire.Spawn is

   --  Encapsulates all external spawns
   --  Any of those may raise Command_Failed or GNAT.OS_Lib exceptions

   procedure Command
     (Cmd                 : String;
      Args                : AAA.Strings.Vector;
      Understands_Verbose : Boolean := False);
   --  Adds -v if understands in Debug log level
   --  Summary is shown after process successful end, if Log_Level = Info

   procedure Settings_Command
     (Cmd          : String;
      Replacements : Alire.Formatting.Replacements;
      Exec_Check   : access procedure (Exec : String) := null);
   --  Launches a command from a string, according to the conventions used by
   --  commands configurable with `alr settings`.
   --
   --  Parses a space-separated string and performs ${} pattern replacements
   --  (on arguments only).
   --
   --  Exec_Check is called with the executable name between parsing Cmd into
   --  arguments and executing the command, e.g. to check that the executable
   --  is available.

   procedure Gprbuild
     (Project_File  : String;
      Extra_Args    : AAA.Strings.Vector);
   --  Launches gprbuild for the building of a crate.
   --  Extra args can be -Xblah detected from command-line.

   procedure Gprinstall
     (Release      : Releases.Release;
      Project_File : Absolute_File;
      Prefix       : Absolute_Path;
      Recursive    : Boolean;
      Quiet        : Boolean;
      Force        : Boolean := Alire.Force;
      Extra_Args   : AAA.Strings.Vector := AAA.Strings.Empty_Vector);
   --  Launches
   --  gprinstall [-f] -m -p [-r] \
   --             --mode=usage -P Project_File --prefix=Prefix -- Extra_Args \
   --             --install-name=Release.Milestone.Image \
   --             --link-lib-dir=Prefix/bin

   function Recreate_Global_Options return AAA.Strings.Vector;
   --  Recreate the global options used in this alr invocation (-v, -n, ...)

end Alire.Spawn;
