with Ada.Directories;

with Alire.Dependencies.Vectors;
with Alire.Os_Lib;
with Alire.Query;

with Alr.Checkout;
with Alr.Commands.Compile;
with Alr.OS;

with Semantic_Versioning;

package body Alr.Commands.Get is

   package Semver renames Semantic_Versioning;

   -------------
   -- Execute --
   -------------

   procedure Execute (Cmd : in out Command) is
      use Ada.Directories;

      Name : constant Alire.Project_Name := Last_Non_Switch_Argument;

      Success : Boolean;
      Needed  : constant Alire.Query.Instance :=
                  Alire.Query.Resolve (Alire.Dependencies.Vectors.New_Dependency (Name, Semver.Any), Success);

      Must_Enter : Boolean;
   begin
      if not Alire.Query.Exists (Name) then
         Log ("Project [" & Name & "] does not exist in the catalog.");
         raise Command_Failed;
      end if;

      if not Success then
         Log ("Failed: could not resolve dependencies.");
         raise Command_Failed;
      end if;

      --  Check if we are already in the fresh copy (may happen after respawning)
      if Bootstrap.Running_In_Session then
         if Bootstrap.Session_Is_Current and then Name = Project.Name then
            Log ("Already in working copy, skipping checkout");
         else
            Log ("Cannot get a project inside another alr session, stopping.");
            raise Command_Failed;
         end if;
         Must_Enter := False;
      else
         Must_Enter := True;
         Checkout.Working_Copy (Needed.Element (Name),
                                Needed,
                                Current_Directory);
         --  Check out requested project under current directory
      end if;

      --  Check out rest of dependencies
      Checkout.To_Folder (Needed, OS.Projects_Folder, But => Name);

      --  Launch build if requested
      if Cmd.Compile then
         declare
            use Alire.OS_Lib;
            Guard : Folder_Guard :=
                      (if Must_Enter
                       then Enter_Folder (Needed.Element (Name).Unique_Folder)
                       else Stay_In_Current_Folder) with Unreferenced;
         begin
            Compile.Execute;
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
                     Cmd.Compile'Access,
                     "-c", "--compile", "Compile after download.");
   end Setup_Switches;

end Alr.Commands.Get;
