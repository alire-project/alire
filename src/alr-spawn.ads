package Alr.Spawn is

   --  Encapsulates all external spawns
   --  Any of those may raise Command_Failed or GNAT.OS_Lib exceptions

   procedure Command (Cmd : String; Args : String := ""; Quiet : Boolean := False);
   --  If Quiet will add a "-q"

   type Gpr_Output is (Raw, Percent, Percent_And_File);

   procedure Gprbuild (Project_File : String; Output : Gpr_Output := Raw);
   --  Builds a progress file

end Alr.Spawn;
