package Alire.Index.Search is

   --  Functions for generating listings of crates/releases

   procedure Print_Crates (Substring : String := "");
   --  Print a list of crates containing Substring in their name/description,
   --  or all of them when empty.

   procedure Print_Dependents (Release    : Releases.Release;
                               Transitive : Boolean;
                               Duplicates : Boolean)
     with Pre => (if not Transitive then not Duplicates);
   --  Print the newest release of a crate that depends on Release. If
   --  Transitive, find also dependents on dependents and so on. If Duplicates,
   --  find all dependency chains; otherwise only the shortest ones and only
   --  one per root dependent.

end Alire.Index.Search;
