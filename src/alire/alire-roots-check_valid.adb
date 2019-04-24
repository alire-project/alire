with Ada.Directories;

function Alire.Roots.Check_Valid (This : Root) return Root is
   use Ada.Directories;
begin
   if not This.Is_Valid then
      return This; -- Keep as is
   elsif not Exists (This.Working_Folder) then
      return New_Invalid_Root.With_Reason ("alire subfolder not found");
   elsif Kind (This.Working_Folder) /= Directory then
      return New_Invalid_Root.With_Reason ("Expected alire folder but found a: " & Kind (This.Working_Folder)'Img);
   elsif not Exists (This.Crate_File) then
      return New_Invalid_Root.With_Reason ("Dependency file not found in alire folder");
   elsif Kind (This.Crate_File) /= Ordinary_File then
      return New_Invalid_Root.With_Reason ("Expected ordinary file but found a: " & Kind (This.Crate_File)'Img);
   else
      return This; -- Nothing untoward detected
   end if;
end Alire.Roots.Check_Valid;
