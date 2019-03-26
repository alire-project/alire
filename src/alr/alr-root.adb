with Ada.Directories;

with Alr.Hardcoded;

package body Alr.Root is

   -----------------
   -- Check_Valid --
   -----------------

   procedure Check_Valid is
      use Ada.Directories;
   begin
      if Is_Empty then
         Trace.Error ("No root project defined despite session being current, check project_alr.ads file");
         raise Command_Failed;
      end if;

      if not Exists (Hardcoded.Alr_Working_Folder) then
         Trace.Error ("alire subfolder not found");
         raise Program_Error;
      elsif Kind (Hardcoded.Alr_Working_Folder) /= Directory then
         Trace.Error ("Expected alire folder but found a: " & Kind (Hardcoded.Alr_Working_Folder)'img);
         raise Program_Error;
      elsif not Exists (Hardcoded.Working_Deps_File) then
         Trace.Error ("Dependency file not found in alire folder");
         raise Program_Error;
      elsif Kind (Hardcoded.Working_Deps_File) /= Ordinary_File then
         Trace.Error ("Expected ordinary file but found a: " & Kind (Hardcoded.Working_Deps_File)'img);
         raise Program_Error;
      end if;
   end Check_Valid;

end Alr.Root;
