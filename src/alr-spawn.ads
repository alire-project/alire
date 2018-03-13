with Alr.Commands;

package Alr.Spawn is

   --  Encapsulates all external spawns
   --  Any of those may raise Command_Failed or GNAT.OS_Lib exceptions

   procedure Alr (Cmd : Commands.Cmd_Names; Args : String := "");
   --  Will append current global switches in addition to Args
   --  Will return after completion!

   procedure Session_Alr_Without_Return (Metafile : String)
     with No_Return;
   --  Forwards execution to the session-specific alr

   procedure Updated_Alr_Without_Return
     with No_Return;
   --  Respawns with same arguments
   --  DOES NOT RETURN!

   procedure Command (Cmd  : String;
                      Args : String := "";
                      Understands_Verbose : Boolean := False;
                      Force_Quiet         : Boolean := False);
   --  Adds -v if understands in Debug log level
   --  Summary is shown after process successful end, if Log_Level = Info

   procedure Gprbuild (Project_File  : String;
                       Session_Build : Boolean; -- If true ALR_BIN will be set
                       Session_Path  : String := "";
                       Extra_Args    : String := "");
   --  Builds a project file, with optional alr metadata file

end Alr.Spawn;
