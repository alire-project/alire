with Alire;

with Alr.Defaults;
with Alr.Devel;
with Alr.OS;
with Alr.OS_Lib; use Alr.OS_Lib;
with Alr.Session;
with Alr.Utils;

package Alr.Bootstrap is

   --  Declarations to enable self-compilation 
   
   Alr_Branch : constant String    := "master";
   Alr_Repo   : constant Alire.URL := Defaults.Alr_Repository;
   
   Alr_Src_Folder : constant String := 
                      (if Devel.Enabled 
                       then OS.Devel_Folder 
                       else OS.Config_Folder / "alr");      
   
   procedure Check_If_Rolling_And_Respawn;
   --  Determines if we are using a rolling release.
   --  If not, and one is available, respawn.
   
   procedure Check_If_Project_Outdated_And_Rebuild;
   --  Check if there is a project file within reach.
   --  If it is, and its hash differs from ours, rebuild.
   
   procedure Check_Rebuild_Respawn
     with Post => (Running_In_Session or else raise Command_Failed);
   --  The whole shebang for project-oriented commands: 
   --    Check within project
   --    Rebuild if outdated
   --    Respawn if rebuilt
   --  Will raise if not within session
   --  FIXME: some kind of infinite respawning prevention should be implemented here
   
   function Running_In_Project return Boolean
     with Pre => Running_In_Session;
   
   function Running_In_Session return Boolean;
   --  Says if there is a project file within reach
   
   function Session_Is_Current return Boolean
     with Pre => Running_In_Session;
   --  Says if our internal session hash matches the one we are in
   
   procedure Rebuild (Alr_File : String := "");
   
   procedure Rebuild_With_Current_Project;
   --  Rebuild, using a single project if in scope
   
   procedure Respawn_With_Canonical (Command_Line : String := Current_Command_Line);
   --  Relaunchs with same command line but using the canonically built executable
   
private
   
   function Running_In_Session return Boolean is
     (OS_Lib.Locate_Any_GPR_File > 0 and then OS_Lib.Locate_Any_Index_File /= "");
   
   function Session_Is_Current return Boolean is
     (Session.Hash = Utils.Hash_File (OS_Lib.Locate_Any_Index_File));   
   
   --  If no project is given the default session is used. 
   --  Otherwise, session file is generated and used
   
--     Semver : constant Release := 
--                Register_Git 
--                  ("semantic_versioning",
--                   V ("1.0.0"),
--                   Defaults.Semver_Repository,
--                   "4f9dd63960cb4040e3aa561019d79e6f9d5f5818");
--     
--     Alire : constant Release :=
--               Register_Git 
--                  ("alire",
--                   V ("0.1.0-alpha"),
--                   Defaults.Index_Repository,
--                   "8265beffb43380a6aa6bf7733bf177f9f03ad55c",
--                   Depends_On => At_Least_Within_Major (Semver));
--     
--     Alr : constant Release :=
--               Register_Git 
--                 ("alr",
--                  V ("0.1.0-alpha"),
--                  Defaults.Alr_Repository,
--                  "2742ae25e757321ba86bbf83b502c39e2dad28c9",
--                  Depends_On => At_Least_Within_Major (Alire));

end Alr.Bootstrap;
