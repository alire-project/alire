function Alire.Roots.Check_Valid (This : Root) return Root
     with Post => Check_Valid'Result.Is_Valid or else
                  Check_Valid'Result.Invalid_Reason /= "";
   --  Check that given Root information is valid (paths, etc)
