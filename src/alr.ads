with Alire;

package Alr with Preelaborate is

   Command_Failed : exception;
   --  Signals "normal" command completion with failure (i.e., no need to print stack trace).

   use all type Alire.Verbosities;

   procedure Log (S : String; Level : Alire.Verbosities := Terse) renames Alire.Log;

end Alr;
