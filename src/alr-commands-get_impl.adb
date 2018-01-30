with Alire.Containers;
with Alire.Depends;
with Alire.Index.Query;

with Semantic_Versioning;

package body Alr.Commands.Get_Impl is

   package Semver renames Semantic_Versioning;

   subtype Releases is Alire.Containers.Release_Set;

   -------------
   -- Execute --
   -------------

   procedure Execute (Cmd : in out Command) is
      Project : constant Alire.Project_Name := Last_Argument;

      Needed  : constant Alire.Containers.Version_Map :=
                 Alire.Index.Query.Resolve (Alire.Depends.New_Dependency (Project, Semver.Any));
   begin
      if not Alire.Index.Query.Exists (Project) then
         Log ("ERROR: project [" & Project & "] does not exist in the catalog.");
         raise Command_Failed;
      end if;

      if Needed.Is_Empty then
         Log ("ERROR: could not resolve dependencies.");
         raise Command_Failed;
      end if;
   end Execute;

   --------------------
   -- Setup_Switches --
   --------------------

   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin
      Define_Switch (Config,
                     Cmd.Build'Access,
                     "", "--build", "Build after download.");
   end Setup_Switches;

end Alr.Commands.Get_Impl;
