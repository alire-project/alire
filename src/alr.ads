with Alire;

package Alr with Preelaborate is

   use all type Alire.Verbosities;

   procedure Log (S : String; Level : Alire.Verbosities := Terse) renames Alire.Log;

end Alr;
