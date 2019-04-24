with Alire.Environment;
with Alire.Root;

with Alr.Platform;

package body Alr.Root is

   -------------
   -- Current --
   -------------

   function Current return Alire.Roots.Root is
   begin
      return Alire.Root.Current
        (Env => Alire.Environment.Setup'
           (OS       => Platform.Operating_System,
            Distro   => Platform.Distribution,
            Compiler => Platform.Compiler));
   end Current;

end Alr.Root;
