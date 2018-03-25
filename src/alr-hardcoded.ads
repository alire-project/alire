with Ada.Directories;

with Alire;

with Alr.Defaults;
with Alr.Platform;
with Alr.Platforms;
with Alr.Self;
with Alr.Utils;

package Alr.Hardcoded is

   --  NOTE: none of the functions in this spec can be used before elaboration is complete

   --  Paths and Files and such that are hardcoded
   --  Also, "hardcoded" paths that are relative to the detected src folder,
   ---   which could be other than the hardcoded canonical one (in devel builds or custom installs)

   Alr_Branch : constant String := "master";
   --  For initial checkouts

   function Alr_Canonical_Folder return String renames Self.Canonical_Folder;
   --  Sources when normally installed

   Alr_Child_Flag : constant String;
   --  Env var set when launching a child alr

   function Alr_Default_Index_File return String;
   --  Minimal index to use when not fully indexing

   function Alr_Gpr_File return String;
   --  Note to self: this is the _env one that works with git submodules

   Alr_Index_File_Base_Name : constant String;
   --  File containing the session index, name only

   function Alr_Index_Folder_Absolute return String;
   --  Path to folder containing all release indexing specification files

   Alr_Repo   : constant Alire.URL := Defaults.Alr_Repository;
   --  RepPlatforms.Currentitory checked out for self-upgrade

   function Alr_Rolling_Exec return String;
   --  Name of the rolling executable, "alr", with source path, for the detected src folder
   --  Might not exist yet!

   function Alr_Src_Default_Session_Folder return String;
   --  Folder for files that are only used if no session-supplied ones exist

   function Alr_Src_Folder return String;
   --  Folder containing sources for rebuild
   --  Either the one for the exe being run, if it could be detected, or the canonical one otherwise

   function Native_Package_List return String;
   --  File containing detected Ada packages in the system package manager
   --  session_folder/packages.txt

   function Scripts_Version return String;

   function Templates_Bin_Folder return String;

   function Templates_Lib_Folder return String;

   --  Functions that return hardcoded-derived files

   function Alire_File (Project : Alire.Project) return String;
   --  File with dependencies (project_alr.ads)

   function Alr_Session_Exec (Metafile : String) return String;
   --  The session-specific built alr

   function Build_File (Project : Alire.Project) return String;
   --  Aggregate project file (project_alr.gpr)

   function Projects_Folder return String;
   --  $CACHE_FOLDER/projects

   function Session_Folder (Metafile : String) return String;
   --  $CACHE_FOLDER/sessions/<pwd>
   --  Also creates it (once it is asked for, it's presumed to be about to be used)
   --  Our anchor is the metadata file

   function No_Session_Folder return String;
   --  The "null" session folder for no-session rebuilds

private

   function "/"    (L, R : String)   return String is     (Ada.Directories.Compose (L, R));
   function Parent (Folder : String) return String renames Ada.Directories.Containing_Directory;

   --  Constants

   Alr_Child_Flag :           constant String := "ALR_CHILD";
   Alr_Index_File_Base_Name : constant String := "alr-index.ads";

   --  Pseudo-constants (due to elaboration finished requirement)

   function Alr_Src_Folder                 return String renames Self.Src_Folder;
   function Alr_Src_Default_Session_Folder return String is (Alr_Src_Folder / "src" / "default_session");
   function Alr_Default_Index_File         return String is (Alr_Src_Default_Session_Folder / "alr-index.ads");
   function Alr_Gpr_File                   return String is (Alr_Src_Folder / "alr_env.gpr");
   function Alr_Index_Folder_Absolute      return String is (Alr_Src_Folder / "deps" / "alire" / "index");
   function Alr_Rolling_Exec               return String is (Alr_Src_Folder / "bin" / "alr");
   function Native_Package_List            return String is (Platform.Config_Folder / "native_packages.txt");
   function No_Session_Folder              return String is (Platform.Cache_Folder / "sessions" / Platform.Compiler'Img / "no_session");
   function Projects_Folder                return String is (Platform.Cache_Folder / "projects" / Platform.Compiler'Img);
   function Scripts_Version                return String is (Alr_Src_Folder / "scripts" / "version");
   function Templates_Bin_Folder           return String is (Alr_Src_Folder / "templates" / "projects" / "bin");
   function Templates_Lib_Folder           return String is (Alr_Src_Folder / "templates" / "projects" / "lib");

   --  Functions

   function Alire_File (Project : Alire.Project) return String is
     (Utils.Head (+Project, Alire.Extension_Separator) & "_alr.ads");

   function Alr_Is_Canonical return Boolean is
     (Platform.Own_Executable = Alr_Canonical_Folder / "bin" / "alr");

   function Alr_Session_Exec (Metafile : String) return String is
     (Session_Folder (Metafile) / "alr");

   function Build_File (Project : Alire.Project) return String is
     (Utils.Head (+Project, Alire.Extension_Separator) & "_alr.gpr");

end Alr.Hardcoded;
