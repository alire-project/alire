with Alire;
with Alire.Containers;
with Alire.Index;

with Alr.Query;

private with Alire.Index.Alire;

package Alr.Bootstrap is

   ---------------------
   --  SESSION STATE  --
   ---------------------

   type Session_States is
     (Erroneous, -- Some bizarre situation
      Outside,   -- not in a project root folder
      Outdated,  -- Running in a project folder, but hashes do not match. Internal Project irrelevant.
      Valid      -- In a session, with matching hash, hence our internal project must match too
                 -- Also, when valid, we are in the project root!
     );
   --  Order in this enum must be in increasing level of available information

   function Session_State return Session_States;

   -------------
   --  OTHER  --
   -------------

   procedure Check_Ada_Tools;
   --  Check gprbuild/gnatmake are within reach

   procedure Check_If_Rolling_And_Respawn;
   --  Determines if we are using a rolling release.
   --  If not, and one is available, respawn.

   procedure Check_Rebuild_Respawn;
   --  The whole shebang for project-oriented commands:
   --    Check within project
   --    Rebuild if outdated
   --    Respawn if rebuilt
   --  Will raise if not within session
   --  FIXME: some kind of infinite respawning prevention should be implemented here

   procedure Rebuild (Alr_File : String := "");

   procedure Rebuild_With_Current_Project;
   --  Rebuild, using a single project if in scope

   function Status_Line return String;
   --  One-liner reporting most interesting information

   Alire_Minimal_Dependency : constant Alire.Index.Release_Dependencies;
   Alire_Minimal_Instance   : constant Query.Instance;

private

   Alire_Minimal_Dependency : constant Alire.Index.Release_Dependencies :=
                                Alire.Index.At_Least (Alire.Index.Alire.V_0_1);
   Alire_Minimal_Instance   : constant Query.Instance :=
                                Alire.Containers.To_Map (Alire.Index.Alire.V_0_1);

   function Running_In_Session return Boolean;
   --  Being inside
   --  Says if there is a project file within reach

   function Session_Is_Current return Boolean;
   --  If we are in a session, says if our internal session hash matches the one we are in

   function Running_In_Project return Boolean;
   --  Extra checks in a current session; failure means some unexpected situation for alire

end Alr.Bootstrap;
