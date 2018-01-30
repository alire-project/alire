with Ada.Directories;

with Alire.Depends;
with Alire.Index;
with Alire.Os_Lib;
with Alire.Query;

with Alr.Commands.Build_Impl;
with Alr.OS;
with Alr.OS_Lib;
with Alr.Templates;

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

      --  Check out requested project under current directory
      declare
         Main_Project : constant Alire.Index.Release := Needed.Element (Project);
      begin
         Main_Project.Checkout (Parent_Folder => Current_Directory);

         --  And generate its dependency file, if it does not exist
         declare
            use Alire.OS_Lib;
            Guard      : Folder_Guard := Enter_Folder (Main_Project.Unique_Folder) with Unreferenced;
            Index_File : constant String := Alr.OS_Lib.Locate_Index_File (Project);
         begin
            if Index_File = "" then
               Templates.Generate_Project_Alire (Needed, Main_Project);
            end if;
         end;
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
         declare
            use Alire.OS_Lib;
            Guard : Folder_Guard := Enter_Folder (Needed.Element (Project).Unique_Folder) with Unreferenced;
         begin
            Build_Impl.Build;
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

end Alr.Commands.Get_Impl;
