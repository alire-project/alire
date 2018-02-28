with Alire;
with Alire.Containers;
with Alire.Index;

with Alr.Query;

private with Alire.Index.Alire;

package Alr.Bootstrap is

   procedure Check_Ada_Tools;
   --  Check gprbuild/gnatmake are within reach

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

   Alire_Minimal_Dependency : constant Alire.Index.Dependencies;
   Alire_Minimal_Instance   : constant Query.Instance;

private

   Alire_Minimal_Dependency : constant Alire.Index.Dependencies :=
                                Alire.Index.At_Least (Alire.Index.Alire.V_0_1);
   Alire_Minimal_Instance   : constant Query.Instance :=
                                Alire.Containers.To_Map (Alire.Index.Alire.V_0_1);

end Alr.Bootstrap;
