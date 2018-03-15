with Alire.Properties.Platform;

with Alr.Platforms.Current;

package body Alr.Platforms is

   use all type Alire.Properties.Vector;

   package Platprop renames Alire.Properties.Platform;

   function Basic_Properties return Alire.Properties.Vector is
     (Platprop.Compiler_Is (Platforms.Compiler) and
          Platprop.Distribution_Is (Platforms.Current.Instance.Distribution) and
          Platprop.System_Is (Platforms.Current.Instance.Operating_System) and
          Platprop.Version_Is (Platforms.Current.Instance.Distro_Version) and
          Platprop.Word_Size_Is (Platforms.Word_Size)
     );

end Alr.Platforms;
