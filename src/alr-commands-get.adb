with Ada.Directories;

with Alire.Depends;
with Alire.Index;
with Alire.Os_Lib;
with Alire.Query;

with Alr.Checkout;
with Alr.Commands.Build;
with Alr.OS;

with Semantic_Versioning;

package body Alr.Commands.Get is

   package Semver renames Semantic_Versioning;

   -------------
   -- Execute --
   -------------

   procedure Execute (Cmd : in out Command) is
      use Ada.Directories;

      Project : constant Alire.Project_Name := Last_Argument;

      Needed  : constant Alire.Index.Instance :=
                 Alire.Query.Resolve (Alire.Depends.New_Dependency (Project, Semver.Any));
   begin
      if not Alire.Query.Exists (Project) then
         Log ("ERROR: project [" & Project & "] does not exist in the catalog.");
         raise Command_Failed;
      end if;

      if Needed.Is_Empty then
         Log ("ERROR: could not resolve dependencies.");
         raise Command_Failed;
      end if;

      Checkout.Working_Copy (Needed.Element (Project),
                             Needed,
                             Current_Directory);
      --  Check out requested project under current directory

      --  Check out rest of dependencies
      Checkout.To_Folder (Needed, OS.Projects_Folder, But => Project);

      --  Launch build if requested
      if Cmd.Build then
         declare
            use Alire.OS_Lib;
            Guard : Folder_Guard := Enter_Folder (Needed.Element (Project).Unique_Folder) with Unreferenced;
         begin
            Build.Execute;
         end;
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

end Alr.Commands.Get;
