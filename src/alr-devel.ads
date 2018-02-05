private with Alr.OS;

private with GNAT.OS_Lib;

package Alr.Devel is

   function Enabled return Boolean;
   --  This global switch changes some behaviors to make for a more friendlier development experience:
   --  Rolling behavior is disabled so there's never respawning of the canonical rolling executable
   --  Self-compilation is launched for the project file in the hard-coded devel folder instead of the canonical one

private

   function Enabled return Boolean is
      (GNAT.OS_Lib.Is_Regular_File (OS.Devel_Telltale));

end Alr.Devel;
