with Alire;
with Alire.Containers;
with Alire.Index;

with Alr.Defaults;
with Alr.Devel;
with Alr.OS;
with Alr.OS_Lib; use Alr.OS_Lib;

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
   --  FIXME: move here the keeping of global switches, now in Commands, so clients haven't to remember to do it

   function Status_Line return String;
   --  One-liner reporting most interesting information

   Alr_Bootstrap_Release  : constant Alire.Index.Release;
   Alr_Minimal_Dependency : constant Alire.Index.Dependencies;
   Alr_Minimal_Instance   : constant Alire.Index.Instance;

private

   use Alire.Index;

   --  Having these public releases enables its inclusion in newly generated projects,
   --  so their project_alr.ads file do really compiles

   Semver_Bootstrap : constant Release :=
              Register_Git
                ("semantic_versioning",
                 V ("1.0.0"),
                 Defaults.Semver_Repository,
                 "4f9dd63960cb4040e3aa561019d79e6f9d5f5818");

   Alire_Bootstrap : constant Release :=
             Register_Git
                ("alire",
                 V ("0.4.0"),
                 Defaults.Index_Repository,
                 "da62fcaec8eab9ac6847140ab9e48f6d2acb5c07",
                 Depends_On => At_Least_Within_Major (Semver_Bootstrap));

   Alr_Bootstrap : constant Release :=
             Register_Git
               ("alr",
                V ("0.3.1"),
                Defaults.Alr_Repository,
                "6193b2399fde40837870e13df342852eca39d974",
                Depends_On => At_Least_Within_Major (Alire_Bootstrap));

   Alr_Minimal_Dependency : constant Alire.Index.Dependencies := At_Least (Alr_Bootstrap);
   Alr_Minimal_Instance   : constant Alire.Index.Instance :=
                              Alire.Containers.To_Map (Alr_Bootstrap);

   Alr_Bootstrap_Release  : constant Release := Alr_Bootstrap;

end Alr.Bootstrap;
