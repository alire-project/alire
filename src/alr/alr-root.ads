with Alire.Root;
with Alire.Roots;

with Alr.Platform;
with Alr.Query;

package Alr.Root is

   function Current return Alire.Roots.Root renames Alire.Root.Current;

   function Is_Empty return Boolean;

   procedure Check_Valid
     with Post => (not Is_Empty or else raise Command_Failed);
   --  Graceful check that Current contains what it should.

   function Is_Indexed return Boolean;
   --  Says if it can be found in the index

   function Platform_Dependencies return Types.Platform_Dependencies;

   function Project return Alire.Project
     with Pre => not Is_Empty;

private

   function Is_Empty return Boolean is (not Alire.Root.Is_Set);

   function Is_Indexed return Boolean is
     (not Is_Empty and then
      Query.Exists (Current.Project, Current.Version));

   function Platform_Dependencies return Types.Platform_Dependencies is
      (Current.Dependencies.Evaluate (Platform.Properties));

   function Project return Alire.Project is (Current.Project);

end Alr.Root;
