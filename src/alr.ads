with Simple_Logging;

package Alr with Preelaborate is

   Command_Failed : exception;
   --  Signals "normal" command completion with failure (i.e., no need to print stack trace).

   use all type Simple_Logging.Levels;

   procedure Log (S : String; Level : Simple_Logging.Levels := Info) renames Simple_Logging.Log;

   package Trace renames Simple_Logging;

end Alr;
