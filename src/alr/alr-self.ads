private with Ada.Directories;

private with Alr.Platform;

package Alr.Self is

   --  Functions relative to self-identification of the current executable

   function Has_Full_Index return Boolean;
   --  Index is complete (all found files at time of rebuild)

   function Is_Bootstrap return Boolean;
   --  Freshly compiled without full index

   function Is_Canonical return Boolean;
   --  Our exe is both rolling and from the canonical source path

   function Is_Rolling return Boolean;
   --  Our exe is from a src folder where it can be recompiled

   function Is_Session return Boolean;
   --  Our exe is specific for a project

   function Matches_Session (Metafile : String) return Boolean;

   function Src_Folder return String;
   --  Check-out containing current executable, or the canonical path if it could not be located

   --  Extra constants needed here to break circularities

   function Canonical_Folder return String;
   --  Where alr sources are located when effectively deployed (not devel compiled)

private

   function "/" (L, R : String) return String is (Ada.Directories.Compose (L, R));

   function Canonical_Folder return String is (Platform.Config_Folder / "alr");
   --  Where alr sources are located when effectively deployed (not devel compiled)

end Alr.Self;
