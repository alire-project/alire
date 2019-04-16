with Ada.Directories;

procedure Alire.Roots.Check_Valid (This : Root) is
   use Ada.Directories;
begin
   if not Exists (This.Working_Folder) then
      Trace.Error ("alire subfolder not found");
      raise Program_Error;
   elsif Kind (This.Working_Folder) /= Directory then
      Trace.Error ("Expected alire folder but found a: " & Kind (This.Working_Folder)'Img);
      raise Program_Error;
   elsif not Exists (This.Crate_File) then
      Trace.Error ("Dependency file not found in alire folder");
      raise Program_Error;
   elsif Kind (This.Crate_File) /= Ordinary_File then
      Trace.Error ("Expected ordinary file but found a: " & Kind (This.Crate_File)'Img);
      raise Program_Error;
   end if;
end Alire.Roots.Check_Valid;
