procedure Alire.Roots.Check_Valid (This : Root)
  with Post => This.Is_Valid or else raise Program_Error;
