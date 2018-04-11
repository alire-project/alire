with Alire.Root;
with Alire.Roots;

with Alr.OS_Lib;

package Alr.Root is

   function Current return Alire.Roots.Root renames Alire.Root.Current;

   function Is_Empty return Boolean;

   procedure Check_Valid
     with Post => (not Is_Empty or else raise Command_Failed);
   --  Graceful check that Current contains what it should.

   function Is_Released return Boolean;

   function Project return Alire.Project
     with Pre => not Is_Empty;

   function Enter_Root return OS_Lib.Folder_Guard
     with Pre => (not Is_Empty);
   --  Enters the root folder if not already there

private

   function Is_Empty return Boolean is (not Alire.Root.Is_Set);

   function Is_Released return Boolean is (Current.Is_Released);

   function Project return Alire.Project is (Current.Project);

end Alr.Root;
