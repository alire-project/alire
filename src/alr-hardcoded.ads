with Alire;

with Alr.Defaults;
with Alr.Self;

private with Ada.Directories;

private with Alr.OS;

package Alr.Hardcoded is

   --  Paths and Files and such that are hardcoded
   --  Also, "hardcoded" paths that are relative to the detected src folder,
   ---   which could be other than the hardcoded canonical one (in devel builds or custom installs)

   Alr_Branch : constant String    := "master";
   --  Branch used to self-upgrade

   Alr_Canonical_Folder : String renames Self.Canonical_Folder;
   --  Sources when normally installed

   Alr_Child_Flag : constant String;
   --  Env var set when launching a child alr

   Alr_Gpr_File : constant String;
   --  Note to self: this is the _env one that works with git submodules

   Alr_Repo   : constant Alire.URL := Defaults.Alr_Repository;
   --  Repository checked out for self-upgrade

   Alr_Rolling_Exe_File : constant String;
   --  Name of the rolling executable, "alr", with source path, for the detected src folder
   --  Might not exist yet!

   Alr_Src_Default_Session_Folder : constant String;
   --  Folder for files that are only used if no session-supplied ones exist

   Alr_Src_Folder : constant String;
   --  Folder containing sources for rebuild
   --  Either the one for the exe being run, if it could be detected, or the canonical one otherwise

   Native_Package_List : constant String;
   --  File containing detected Ada packages in the system package manager
   --  session_folder/packages.txt

   Scripts_Apt_Detect : constant String;

   Scripts_Version : constant String;

   Templates_Bin_Folder : constant String;

   Templates_Lib_Folder : constant String;

   --  Functions that return hardcoded-derived files

   function Alire_File (Project : Alire.Name_String) return String;
   --  File with dependencies (project_alr.ads)

   function Build_File (Project : Alire.Name_String) return String;
   --  Aggregate project file (project_alr.gpr)

   function Projects_Folder return String;
   --  $CACHE_FOLDER/projects

   function Session_Folder return String;
   --  $CACHE_FOLDER/sessions/<pwd>
   --  Also creates it (once it is asked for, it's presumed to be about to be sued)

private

   function "/" (L, R : String) return String is (Ada.Directories.Compose (L, R));
   function Parent (Folder : String) return String renames Ada.Directories.Containing_Directory;

   Alr_Child_Flag : constant String := "ALR_CHILD";

   Alr_Src_Folder : constant String := Self.Src_Folder;

   Alr_Src_Default_Session_Folder : constant String := Alr_Src_Folder / "src" / "default_session";

--     Alr_Conf_File  : constant String := "alr-config.ads";

   Alr_Gpr_File         : constant String := Alr_Src_Folder / "alr_env.gpr";

   Alr_Rolling_Exe_File : constant String := Alr_Src_Folder / "bin" / "alr";

   Native_Package_List : constant String := OS.Config_Folder / "native_packages.txt";

   Scripts_Apt_Detect : constant String := Alr_Src_Folder / "scripts" / "aptdetect";

   Scripts_Version : constant String := Alr_Src_Folder / "scripts" / "version";

   Templates_Bin_Folder : constant String := Alr_Src_Folder / "templates" / "projects" / "bin";

   Templates_Lib_Folder : constant String := Alr_Src_Folder / "templates" / "projects" / "lib";

   --  Function bodies

   function Alire_File (Project : Alire.Name_String) return String is
     (Project & "_alr.ads");

   function Alr_Is_Canonical return Boolean is
      (OS.Own_Executable = Alr_Canonical_Folder / "bin" / "alr");

   function Build_File (Project : Alire.Name_String) return String is
     (Project & "_alr.gpr");

   function Projects_Folder return String is
     (OS.Cache_Folder / "projects" / OS.Compiler'Img);

end Alr.Hardcoded;
