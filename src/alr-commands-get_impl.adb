with Ada.Directories;

with Alire.Depends;
with Alire.Index.Query;

with Alr.OS;

with Semantic_Versioning;

package body Alr.Commands.Get_Impl is

   package Semver renames Semantic_Versioning;

   -------------
   -- Execute --
   -------------

   procedure Execute (Cmd : in out Command) is
      use Ada.Directories;

      Project : constant Alire.Project_Name := Last_Argument;

      Needed  : constant Alire.Index.Instance :=
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

      --  Check out requested project under current directory
      begin
         Needed.Element (Project).Checkout (Parent_Folder => Current_Directory);
      exception
         when Alire.File_Error =>
            --  We'll presume it's already there and OK
            Log ("Skipping checkout for already available " & Needed.Element (Project).Milestone_Image);
      end;

      --  Check out rest of dependencies
      for Rel of Needed loop
         if Rel.Project /= Project then
            begin
               Rel.Checkout (Parent_Folder => Alr.OS.Projects_Folder);
            exception
               when Alire.File_Error =>
                  --  We'll presume it's already there and OK
                  Log ("Skipping checkout for already available " & Rel.Milestone_Image);
            end;
         end if;
      end loop;

      --  Launch build if requested
      if Cmd.Build then
         null;
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
