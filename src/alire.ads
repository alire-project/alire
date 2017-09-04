with GNAT.IO; -- For debugging purposes, FIXME getting rid of it and using some proper Trace lib

package Alire with Preelaborate is

   procedure Log (S : String) renames GNAT.IO.Put_Line;

end Alire;
