package Alr.Spawn is

   --  Encapsulates all external spawns
   --  Any of those may raise Command_Failed or GNAT.OS_Lib exceptions

   procedure Command (Cmd  : String;
                      Args : String := "";
                      Understands_Verbose : Boolean := False;
                      Force_Quiet         : Boolean := False);
   --  Adds -v if understands in Debug log level
   --  Summary is shown after process successful end, if Log_Level = Info

   procedure Gprbuild (Project_File  : String;
                       Extra_Args    : String := "");

end Alr.Spawn;
