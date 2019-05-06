with Ada.Directories;

with Alire;
with Alire.Config;
with Alire.Environment;

with Alr.Defaults;
with Alr.OS_Lib;

with GNAT.OS_Lib;

package Alr.Paths is

   --  NOTE: none of the functions in this spec can be used before elaboration
   --  is complete.

   --  TODO: this note will be obsolete once Alire.Platform supersedes
   --  Alr.Platform.

   function "/" (L, R : String) return String;
   --  Equivalent to Ada.Directories.Compose

   function Parent (Folder : String) return String
   renames Ada.Directories.Containing_Directory;

   function Is_Simple_Name (Path : String) return Boolean
   is (for all C of Path => C /= GNAT.OS_Lib.Directory_Separator);

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


   Alr_Repo : constant Alire.URL := Defaults.Alr_Repository;
   --  Repository where alr sources can be found

   function Alr_Config_Folder return Absolute_Path;
   --  Root folder containing persistent configuration: indexes, templates

   function Alr_Source_Folder return Absolute_Path;
   --  Folder inside Alr_Config_Folder containing a clone of the alr repo
   --  This folder can be overriden via environment variable
   --  Alire.Environment.Source.

   function Alr_Index_Folder return Absolute_Path;
   --  Folder inside Alr_Src_Folder containing the defaul public index
   --  TODO: obsolete this once "alr index" command is in place

   Alr_Working_Folder : constant Relative_Path;
   --  Folder within a working project that will contain metadata/build files,
   --  3rd-party projects, and session.

   Alr_Working_Cache_Folder : constant Relative_Path;
   --  Folder inside the working folder with transient files (can be safely
   --  deleted).

   Alr_Working_Deps_Path : constant Relative_Path;
   --  Path from inside the working folder to dependency folders

   function Templates_Bin_Folder return Absolute_Path;

   function Templates_Lib_Folder return Absolute_Path;

   --  Functions that return Paths-derived files

   function Projects_Folder return Relative_Path;
   --  $ALR_WORKING_FOLDER/projects


   --  Scripts paths
   Scripts_Graph_Easy : constant String := "graph-easy";

private

   function "/" (L, R : String) return String
   is (Ada.Directories.Compose (L, R));

   --  Constants

   Alr_Working_Folder       : constant String := "alire";

   Alr_Working_Cache_Folder : constant Relative_Path :=
     Alr_Working_Folder / "cache";

   Alr_Working_Deps_Path    : constant Relative_Path := "cache" / "projects";

   --  Pseudo-constants (due to elaboration finished requirement)
   --  Or because they can be set after elaboration (e.g. via config switches)

   function Alr_Config_Folder return String
   is (Alire.Config.Path);

   function Alr_Source_Folder return String
   is (OS_Lib.Getenv (Alire.Environment.Source, Alr_Config_Folder / "alire"));

   function Alr_Index_Folder return Absolute_Path
   is (Alr_Source_Folder / "deps" / "alire-index" / "index");

   function Projects_Folder      return String
   is (Alr_Working_Cache_Folder / "projects");

   function Session_Folder       return String
   is (Alr_Working_Cache_Folder / "session");

   function Templates_Bin_Folder return String
   is (Alr_Source_Folder / "templates" / "projects" / "bin");

   function Templates_Lib_Folder return String
   is (Alr_Source_Folder / "templates" / "projects" / "lib");

end Alr.Paths;
