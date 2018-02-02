with Ada.Directories;

with Alire.Index;
with Alire.OS_Lib;
with Alire.Query;
with Alire.Releases;
with Alire.Repositories.Local;

with Alr.Bootstrap;
with Alr.Commands.Build;
with Alr.OS_Lib;
with Alr.Templates;
with Alr.Utils;

with Semantic_Versioning; use Semantic_Versioning;

package body Alr.Commands.Init is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      Name : constant String := Last_Argument;
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

      --  Create and enter folder for generation
      if Cmd.No_Skel then
         Ada.Directories.Create_Directory (Name);
      else
         declare
            use OS_Lib;
         begin
            OS_Lib.Copy (Bootstrap.Alr_Src_Folder / "templates" / "projects" /
                         	(if Cmd.Bin then "bin" else "lib"),
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
         Guard : constant Folder_Guard := Alire.OS_Lib.Enter_Folder (Name) with Unreferenced;

         New_Release : constant Alire.Releases.Release :=
                          Alire.Releases.New_Release (Name, V ("0.0.0"),
                                                      Alire.Repositories.Local.Repo,
                                                      Alire.Repositories.Local.Local_Id,
                                                      Depends_On => Bootstrap.Alr_Minimal_Dependency);
         Success : Boolean;
         Depends : constant Alire.Index.Instance := Alire.Query.Resolve (New_Release.Depends, Success);
      begin
         if not Success then
            raise Program_Error with "Alr could not resolve its own dependency, this should never happen!";
         end if;

         Templates.Generate_Project_Alire (Bootstrap.Alr_Minimal_Instance, New_Release, Exact => False);
         Templates.Generate_Gpr (Depends, New_Release);

         Log ("Project initialization completed");
         if Cmd.Build then
            Commands.Build.Execute (Online => False);
         else
            Log ("You may now enter its folder and issue ""alr build""");
         end if;
      end;
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
