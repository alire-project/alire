with Alire;
with Alire.Containers;
with Alire.Index;
with Alire.Query;

with Alr.Defaults;

package Alr.Bootstrap is

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

   function Status_Line return String;
   --  One-liner reporting most interesting information

   Alr_Bootstrap_Release  : constant Alire.Index.Release;
   Alr_Minimal_Dependency : constant Alire.Index.Dependencies;
   Alr_Minimal_Instance   : constant Alire.Query.Instance;

private

   use Alire.Index;
   use all type Alire.Index.Dependencies;

   --  Having these public releases enables its inclusion in newly generated projects,
   --  so their project_alr.ads file do really compiles

   Semver_Bootstrap : constant Release :=
              Register_Git
                ("semantic_versioning",
                 V ("1.0.0"),
                 "Semantic Versioning for Ada",
                 Defaults.Semver_Repository,
                 "4f9dd63960cb4040e3aa561019d79e6f9d5f5818");

   Simple_Logging_Bootstrap : constant Release :=
                        Register_Git
                          ("simple_logging",
                           V ("1.0.0"),
                           "Basic logging to console",
                           Defaults.Simple_Logging_Repo,
                           "77896e4a9d0539a63e6bfb657ab955656c2e3c0f");

   Alire_Bootstrap : constant Release :=
             Register_Git
                ("alire",
                 V ("0.4.0"),
                 "Alire catalog of Ada libraries",
                 Defaults.Index_Repository,
                 "da62fcaec8eab9ac6847140ab9e48f6d2acb5c07",
                 Depends_On =>
                   At_Least_Within_Major (Semver_Bootstrap)  and
                   At_Least_Within_Major (Simple_Logging_Bootstrap));

   Alr_Bootstrap : constant Release :=
             Register_Git
               ("alr",
                V ("0.4.0"),
                "Alire tool to interact with the catalog",
                Defaults.Alr_Repository,
                "146fe156caf0c74978dd7db07585159da0432359",
                Depends_On =>
                  At_Least_Within_Major (Alire_Bootstrap) and
                  At_Least_Within_Major (Simple_Logging_Bootstrap));

   Alr_Minimal_Dependency : constant Alire.Index.Dependencies := At_Least (Alr_Bootstrap);
   Alr_Minimal_Instance   : constant Alire.Query.Instance :=
                              Alire.Containers.To_Map (Alr_Bootstrap);

   Alr_Bootstrap_Release  : constant Release := Alr_Bootstrap;

end Alr.Bootstrap;
