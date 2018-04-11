with Ada.Directories;

with Alire;

with Alr.Defaults;
with Alr.Platform;
with Alr.Platforms;
with Alr.Self;

package Alr.Hardcoded is

   --  NOTE: none of the functions in this spec can be used before elaboration is complete

   --  Paths and Files and such that are hardcoded
   --  Also, "hardcoded" paths that are relative to the detected src folder,
   ---   which could be other than the hardcoded canonical one (in devel builds or custom installs)


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

   function Alr_Canonical_Folder return Absolute_Path renames Self.Canonical_Folder;
   --  Sources when normally installed

   Alr_Child_Flag : constant String;
   --  Env var set when launching a child alr

   function Alr_Default_Index_File return Absolute_File;
   --  Minimal index to use when not fully indexing

   function Alr_Selfbuild_Gpr_File return Absolute_Path;
   --  Note to self: this is the _env one that works with git submodules

   function Alr_Index_Folder return Absolute_Path;
   --  Path to folder containing all release indexing specification files

   Alr_Repo : constant Alire.URL := Defaults.Alr_Repository;
   --  Repository checked out for self-upgrade

   function Alr_Rolling_Exec return Absolute_File;
   --  Name of the rolling executable, "alr", with source path, for the detected src folder
   --  Might not exist yet!

   function Alr_Src_Default_Session_Folder return Absolute_Path;
   --  Folder for files that are only used if no session-supplied ones exist

   function Alr_Src_Folder return Absolute_Path;
   --  Folder containing sources for rebuild
   --  Either the one for the exe being run, if it could be detected, or the canonical one otherwise

   Alr_Working_Folder : constant Relative_Path;
   --  Folder within a working project that will contain metadata/build files, 3rd-party projects, and session

   function Scripts_Version return String;

   function Templates_Bin_Folder return Absolute_Path;

   function Templates_Lib_Folder return Absolute_Path;

   --  Functions that return hardcoded-derived files

   function Alr_Session_Exec return Relative_File;
   --  The session-specific built alr

   function Working_Deps_File return Relative_File;
   --  File with dependencies ($alr_working_folder/alr_deps.ads)

   function Working_Build_File return Relative_File;
   --  Aggregate project file ($alr_working_folder/alr_build.gpr)

   function Projects_Folder return Relative_Path;
   --  $ALR_WORKING_FOLDER/projects

   function Session_Folder return Relative_Path;
   --  $ALR_WORKING_FOLDER/session
   --  Also creates it (once it is asked for, it's presumed to be about to be used)
   --  Our anchor is the metadata file

   function No_Session_Folder return Absolute_Path;
   --  The "null" session folder for no-session rebuilds

private

   function "/"    (L, R : String)   return String is     (Ada.Directories.Compose (L, R));
   function Parent (Folder : String) return String renames Ada.Directories.Containing_Directory;

   --  Constants

   Alr_Child_Flag             : constant String := "ALR_CHILD";
   Alr_Working_Folder         : constant String := "alire";

   --  Pseudo-constants (due to elaboration finished requirement)

   function Alr_Src_Folder                 return String renames Self.Src_Folder;
   function Alr_Src_Default_Session_Folder return String is (Alr_Src_Folder / "src" / "default_session");
   function Alr_Default_Index_File         return String is (Alr_Src_Default_Session_Folder / "alr-index.ads");
   function Alr_Selfbuild_Gpr_File         return String is (Alr_Src_Folder / "alr_env.gpr");
   function Alr_Index_Folder               return String is (Alr_Src_Folder / "deps" / "alire" / "index");
   function Alr_Rolling_Exec               return String is (Alr_Src_Folder / "bin" / "alr");
   function No_Session_Folder              return String is (Platform.Cache_Folder / "sessions" / Platform.Compiler'Img / "no_session");
   function Projects_Folder                return String is (Alr_Working_Folder / "cache" / "projects");
   function Scripts_Version                return String is (Alr_Src_Folder / "scripts" / "version");
   function Session_Folder                 return String is (Alr_Working_Folder / "cache" / "session");
   function Templates_Bin_Folder           return String is (Alr_Src_Folder / "templates" / "projects" / "bin");
   function Templates_Lib_Folder           return String is (Alr_Src_Folder / "templates" / "projects" / "lib");

   --  Functions

   function Working_Deps_File return String is
     (Alr_Working_Folder / "alr_deps.ads");

   function Alr_Is_Canonical return Boolean is
     (Platform.Own_Executable = Alr_Canonical_Folder / "bin" / "alr");

   function Alr_Session_Exec return String is
     (Session_Folder / "alr");

   function Working_Build_File return String is
     (Alr_Working_Folder / "alr_build.gpr");

end Alr.Hardcoded;
