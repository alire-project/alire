with Ada.Directories;

with Alire.Index;
with Alire.Origins;
with Alire.Query;
with Alire.Releases;

with Alr.Bootstrap;
with Alr.Hardcoded;
with Alr.OS_Lib;
with Alr.Spawn;
with Alr.Templates;
with Alr.Utils;

with Semantic_Versioning; use Semantic_Versioning;

package body Alr.Commands.Init is

   --------------
   -- Generate --
   --------------

   procedure Generate (Cmd : Command) is
      Name : constant String := Last_Non_Switch_Argument;
   begin
      if Cmd.No_Skel then
         Ada.Directories.Create_Directory (Name);
      else
         declare
            use OS_Lib;
         begin
            OS_Lib.Copy_File ((if Cmd.Bin
                               then Hardcoded.Templates_Bin_Folder
                               else Hardcoded.Templates_Lib_Folder),
                              Name);
         end;

         OS_Lib.Sed_Folder (Name,
                            Utils.To_Lower_Case (Templates.Sed_Pattern),
                            Utils.To_Lower_Case (Name));
         OS_Lib.Sed_Folder (Name,
                            Utils.To_Mixed_Case (Templates.Sed_Pattern),
                            Utils.To_Mixed_Case (Name));
      end if;

      declare
         Guard : constant Folder_Guard := OS_Lib.Enter_Folder (Name) with Unreferenced;

         New_Release : constant Alire.Releases.Release :=
                         Alire.Releases.New_Release (Name,
                                                     "No description",
                                                     V ("0.0.0-alr_working_copy"),
                                                     Alire.Origins.New_Filesystem (Ada.Directories.Current_Directory),
                                                     Depends_On => Bootstrap.Alire_Minimal_Dependency,
                                                     Properties => Alire.Index.Default_Properties,
                                                     Requisites => Alire.Index.No_Requisites,
                                                     Native     => False);
         Success : Boolean;
         Depends : constant Alire.Query.Instance := Alire.Query.Resolve (New_Release.Depends, Success);
      begin
         if not Success then
            raise Program_Error with "Alr could not resolve its own dependency, this should never happen!";
         end if;

         Templates.Generate_Prj_Alr (Bootstrap.Alire_Minimal_Instance, New_Release, Exact => False);
         Templates.Generate_Agg_Gpr (Depends, New_Release);
      end;
   end Generate;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      Name : constant String := Last_Non_Switch_Argument;
   begin
      if not (Cmd.Bin or Cmd.Lib) then
         Log ("Please provide either --bin or --lib");
         raise Command_Failed;
      end if;

      if Utils.To_Lower_Case (Name) = Utils.To_Lower_Case (Templates.Sed_Pattern) then
         Log ("The project name is invalid, as it is used internally by alr; please choose another name");
         raise Command_Failed;
      end if;

      if Ada.Directories.Exists (Name) then
         Log ("Folder " & Utils.Quote (Name) & " already exists, not proceeding.");
         raise Command_Failed;
      end if;

      --  Create and enter folder for generation, if it didn't happen already
      if Bootstrap.Running_In_Session then
         if Bootstrap.Session_Is_Current and then Name = Project.Name then
            Log ("Already in working copy, skipping initialization");
         else
            Log ("Cannot initialize a project inside another alr project, stopping.");
            raise Command_Failed;
         end if;
      else
         Generate (Cmd);
         Log ("Project initialization completed");
      end if;

      if Cmd.Build then
         declare
            Guard : constant Folder_Guard := OS_Lib.Enter_Folder (Name) with Unreferenced;
         begin
            Spawn.Alr (Cmd_Build);
         end;
      end if;
   end Execute;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin
      Define_Switch (Config,
                     Cmd.Bin'Access,
                     "", "--bin",
                     "New project is an executable");

      Define_Switch (Config,
                     Cmd.Lib'Access,
                     "", "--lib",
                     "New project is a library");

      Define_Switch (Config,
                     Cmd.Build'Access,
                     "-b", "--build",
                     "Enter project and build it after initialization");

      Define_Switch (Config,
                     Cmd.No_Skel'Access,
                     "-n", "--no-skel",
                     "Do not generate non-alire skeleton files");
   end Setup_Switches;

end Alr.Commands.Init;
