with Alr.Commands;

package Alr.Spawn is

   --  Encapsulates all external spawns
   --  Any of those may raise Command_Failed or GNAT.OS_Lib exceptions

   procedure Alr (Cmd : Commands.Cmd_Names; Args : String := "");
   pragma Compile_Time_Warning (True, "Default switchs should'nt be reappended");
   --  Will append current global switches in addition to Args
   --  Will return after completion!

   procedure Updated_Alr_Without_Return
     with No_Return;
   --  Respawns with same arguments
   --  DOES NOT RETURN!

   procedure Command (Cmd : String; Args : String := ""; Quiet : Boolean := Commands.Is_Quiet);
   --  If Quiet will add a "-q"

   type Gpr_Output is (Quiet, Percent, Percent_And_File, Raw);

   procedure Gprbuild (Project_File : String;
                       Session_File : String := "";
                       Output       : Gpr_Output := (if Commands.Is_Quiet then Quiet else Raw));
   --  Builds a progress file

end Alr.Spawn;
