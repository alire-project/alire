with Alire;
with Alire.Index;

private with Alire.Index.Alire;

package Alr.Bootstrap is

   type Rebuild_Types is (Standalone, Session);
   --  Standalone: main alr that gets called first. If we are inside a session, the session one is called
   --  Session: a session-specific alr with compiled-in project metadata

   ---------------------
   --  SESSION STATE  --
   ---------------------

   type Session_States is
     (Erroneous, -- Some bizarre situation
      Outside,   -- not in a project root folder
      Detached,  -- In project folder, but current executable is not a session-specific one
      Valid      -- In a session, running from the session alr, hence our internal project must match too
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
   --  An outdated session-specific build should never be launched

   procedure Rebuild (Kind : Rebuild_Types);

   procedure Rebuild_Respawn (Kind : Rebuild_Types);
   --  Forces a rebuild and respawns, with or without session

   function Status_Line return String;
   --  One-liner reporting most interesting information

   Alire_Minimal_Dependency : constant Alire.Index.Release_Dependencies;

private

   Is_Child : Boolean := False;
   --  During elaboration this will be updated accordingly

   Alire_Minimal_Dependency : constant Alire.Index.Release_Dependencies := Alire.Index.Alire.Project.Current;

end Alr.Bootstrap;
