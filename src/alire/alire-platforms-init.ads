package Alire.Platforms.Init with Elaborate_Body is

   --  Platform-specific initialization code like e.g. msys2 installation.

   --  This package is segregated from Alire.Platforms.Current because
   --  otherwise it introduces elaboration cycles, as e.g. Windows msys2
   --  setup requires packages that themselves depend on the current
   --  platform properties.

end Alire.Platforms.Init;
