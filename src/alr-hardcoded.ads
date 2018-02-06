with Alire.OS_Lib; use Alire.OS_Lib;

with Alr.Defaults;

private with Alr.Devel;
private with Alr.OS;

package Alr.Hardcoded is

   --  Paths and Files and such that are hardcoded

   Alr_Branch : constant String    := "master";
   --  Branch used to self-upgrade

   Alr_Repo   : constant Alire.URL := Defaults.Alr_Repository;
   --  Repository checked out for self-upgrade

   Alr_Src_Folder : constant String;
   --  when Devel.Enabled => User_Folder/local/alr
   --                else => Config_Folder/alire/alr

   Template_Metadata : constant String;
   --  Alr_Src_Folder/templates/files/project_skel_alr.ads

private

   Alr_Src_Folder : constant String := (if Devel.Enabled
                                        then OS.Devel_Folder
                                        else OS.Config_Folder / "alr");

   Template_Metadata : constant String := Alr_Src_Folder / "templates" / "files" / "project_skel_alr.ads";

end Alr.Hardcoded;
