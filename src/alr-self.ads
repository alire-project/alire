private with Ada.Directories;

private with Alr.OS;

package Alr.Self is

   --  Functions relative to self-identification of the current executable

   function Is_Bootstrap return Boolean;
   --  Freshly compiled without full index

   function Is_Canonical return Boolean;
   --  Our exe is both rolling and from the canonical source path

   function Is_Rolling return Boolean;
   --  Our exe is from a src folder where it can be recompiled

   function Src_Folder return String;
   --  Check-out containing current executable, or the canonical path if it could not be located

   --  Extra constants needed here to break circularities

   Canonical_Folder : constant String;
   --  Where alr sources are located when effectively deployed (not devel compiled)

private

   function "/" (L, R : String) return String is (Ada.Directories.Compose (L, R));

   Canonical_Folder     : constant String := OS.Config_Folder / "alr";
   --  Where alr sources are located when effectively deployed (not devel compiled)

end Alr.Self;
