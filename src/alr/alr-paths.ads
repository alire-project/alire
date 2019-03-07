with Ada.Directories;

with Alire;

with Alr.Defaults;
with Alr.Environment;
with Alr.Platform;
with Alr.OS_Lib;

package Alr.Paths is

   --  NOTE: none of the functions in this spec can be used before elaboration is complete


   --  To clarify constants/functions declared herein:

   subtype Absolute_File is String;
   --  Filenames with full path

   subtype Absolute_Path is String;

   subtype Relative_File is String;
   --  Filenames with relative paths

   subtype Relative_Path is String;
   --  A relative path

   subtype Simple_File is String;
   --  Filenames without path


   --  Now, the declarations:

   Alr_Branch : constant String := "master";
   --  For initial checkouts

   Alr_Repo : constant Alire.URL := Defaults.Alr_Repository;
   --  Repository checked out for self-upgrade

   Alr_Working_Folder : constant Relative_Path;
   --  Folder within a working project that will contain metadata/build files, 3rd-party projects, and session

   Alr_Working_Cache_Folder : constant Relative_Path;
   --  Folder inside the working folder with transient files (can be safely deleted)

   Alr_Working_Deps_Path : constant Relative_Path;
   --  Path from inside the working folder to dependency folders

   function Alr_Default_Src_Folder return Absolute_Path;
   --  The default folder where alr is checked out if not overriden with
   --    Environment.Alr_Src_Folder

   function Alr_Src_Folder return Absolute_Path;
   --  A folder where current alr sources are checked out.
   --  These are needed not for recompilation anymore, but for scripts and templates

   function Scripts_Git_Fingerprint return String;

   function Templates_Bin_Folder return Absolute_Path;

   function Templates_Lib_Folder return Absolute_Path;

   --  Functions that return Paths-derived files

   function Working_Deps_File return Relative_File;
   --  File with dependencies ($alr_working_folder/alr_deps.ads)

   function Working_Build_File return Relative_File;
   --  Aggregate project file ($alr_working_folder/alr_build.gpr)

   function Projects_Folder return Relative_Path;
   --  $ALR_WORKING_FOLDER/projects


   --  Other stuff
   Scripts_Graph_Easy : constant Simple_File := "graph-easy";

private

   function "/"    (L, R : String)   return String is     (Ada.Directories.Compose (L, R));
   function Parent (Folder : String) return String renames Ada.Directories.Containing_Directory;

   --  Constants

   Alr_Working_Folder         : constant String := "alire";
   Alr_Working_Cache_Folder   : constant Relative_Path := Alr_Working_Folder / "cache";
   Alr_Working_Deps_Path      : constant Relative_Path := "cache" / "projects";

   --  Pseudo-constants (due to elaboration finished requirement)

   function Alr_Default_Src_Folder return Absolute_Path renames
     Platform.Config_Folder;

   function Alr_Src_Folder return String is
     (OS_Lib.Getenv (Environment.Alr_Src_Folder,
                     Alr_Default_Src_Folder));

   function Projects_Folder                return String is (Alr_Working_Cache_Folder / "projects");
   function Scripts_Git_Fingerprint        return String is (Alr_Src_Folder / "scripts" / "git-fingerprint");
   function Session_Folder                 return String is (Alr_Working_Cache_Folder / "session");
   function Templates_Bin_Folder           return String is (Alr_Src_Folder / "templates" / "projects" / "bin");
   function Templates_Lib_Folder           return String is (Alr_Src_Folder / "templates" / "projects" / "lib");

   --  Functions

   function Working_Deps_File return String is
     (Alr_Working_Folder / "alr_deps.ads");

   function Working_Build_File return String is
     (Alr_Working_Folder / "alr_build.gpr");

end Alr.Paths;
