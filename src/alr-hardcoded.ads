with Alr.Defaults;

private with Alire.Os_Lib;

private with Alr.Devel;
private with Alr.OS;

package Alr.Hardcoded is

   --  Paths and Files and such that are hardcoded

   Alr_Branch : constant String    := "master";
   --  Branch used to self-upgrade

   Alr_Default_Session_Folder : constant String;

   Alr_Exe_File : constant String;

   Alr_Gpr_File : constant String;
   --  Note to self: this is the _env one that works with git submodules

   Alr_Repo   : constant Alire.URL := Defaults.Alr_Repository;
   --  Repository checked out for self-upgrade

   Alr_Src_Folder : constant String;
   --  when Devel.Enabled => User_Folder/local/alr
   --                else => Config_Folder/alire/alr

   Scripts_Version : constant String;

   Templates_Bin_Folder : constant String;

   Templates_Lib_Folder : constant String;

private

   use Alire.OS_Lib;

   Alr_Src_Folder : constant String := (if Devel.Enabled
                                        then OS.Devel_Folder
                                        else OS.Config_Folder / "alr");

   Alr_Default_Session_Folder : constant String := Alr_Src_Folder / "src" / "default_session";

   Alr_Exe_File : constant String := Alr_Src_Folder / "bin" / "alr";

   Alr_Gpr_File : constant String := Alr_Src_Folder / "alr_env.gpr";

   Scripts_Version : constant String := Alr_Src_Folder / "scripts" / "version";

   Templates_Bin_Folder : constant String := Alr_Src_Folder / "templates" / "projects" / "bin";

   Templates_Lib_Folder : constant String := Alr_Src_Folder / "templates" / "projects" / "lib";

end Alr.Hardcoded;
