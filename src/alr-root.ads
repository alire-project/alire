with Alire.Root;
with Alire.Roots;

with Alr.Platform;

package Alr.Root is

   function Current return Alire.Roots.Root renames Alire.Root.Current;

   function Is_Empty return Boolean;

   procedure Check_Valid
     with Post => (not Is_Empty or else raise Command_Failed);
   --  Graceful check that Current contains what it should.

   function Is_Released return Boolean;

   function Platform_Dependencies return Types.Platform_Dependencies;

   function Project return Alire.Project
     with Pre => not Is_Empty;

private

   function Is_Empty return Boolean is (not Alire.Root.Is_Set);

   function Is_Released return Boolean is (Current.Is_Released);

   function Platform_Dependencies return Types.Platform_Dependencies is
      (Current.Dependencies.Evaluate (Platform.Properties));

   function Project return Alire.Project is (Current.Project);

end Alr.Root;
