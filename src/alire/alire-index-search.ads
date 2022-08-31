package Alire.Index.Search is

   --  Functions for generating listings of crates/releases

   procedure Print_Crates (Substring : String := "");
   --  Print a list of crates containing Substring in their name/description,
   --  or all of them when empty.

   procedure Print_Dependents (Release : Releases.Release);
   --  Print the newest release of a crate that depends on Release

end Alire.Index.Search;
