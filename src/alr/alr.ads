with Alire;
with Alire.Types;

with Simple_Logging;

package Alr with Preelaborate is

   --  Nothing of note in this root package
   --  Entities declared here are generally useful everywhere or in many packages:
   --  Exceptions for commands, tracing for all

   Child_Failed   : exception;
   --  Used to notify that a subprocess completed with non-zero error

   Command_Failed : exception;
   --  Signals "normal" command completion with failure (i.e., no need to print stack trace).

   pragma Warnings (Off);
   use all type Alire.Project;
   pragma Warnings (On);

   use all type Simple_Logging.Levels;

   procedure Log (S : String; Level : Simple_Logging.Levels := Info) renames Simple_Logging.Log;

   package Trace renames Simple_Logging;

   package Types renames Alire.Types;

   --  Some hardcoded constants that help to break circularities

   Bootstrap_Hash : constant String := "bootstrap";

end Alr;
