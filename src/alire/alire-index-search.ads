package Alire.Index.Search is

   --  Functions for generating listings of crates/releases

   procedure Print_Crates (Substring : String := "");
   --  Print a list of crates containing Substring in their name/description,
   --  or all of them when empty.

end Alire.Index.Search;
