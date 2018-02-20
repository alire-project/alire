with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with Alire.Dependencies.Vectors;
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
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;

      --  Requested project with optional restriction
      Request : constant String := Last_Non_Switch_Argument;

      --  Locate and identify the version operator
      Op_Pos  : constant Natural := Index (Request, To_Set ("=^~"), Inside);

      --  Ready to separate name from version, and operator if existing
      Name    : constant Alire.Project_Name := (if Op_Pos > Request'First
                                                then Request (Request'First .. Op_Pos - 1)
                                                else Request);

      Op      : constant Character := (if Op_Pos > Request'First
                                       then Request (Op_Pos)
                                       else ASCII.NUL);

      V       : constant Semver.Version := (if Op_Pos > Request'First
                                            then Semver.Relaxed (Request (Op_Pos + 1 .. Request'Last))
                                            else Semver.V ("0.0.0"));

      Versions : constant Semver.Version_Set := (case Op is
                                           when ASCII.NUL => Semver.Any,
                                           when '='       => Semver.Exactly (V),
                                           when '^'       => Semver.Within_Major (V),
                                           when '~'       => Semver.Within_Minor (V),
                                           when others    => raise Wrong_Command_Arguments with "Unrecognized version operator: " & Op);

      Success : Boolean;
      Needed  : constant Alire.Query.Instance :=
                  Alire.Query.Resolve (Alire.Dependencies.Vectors.New_Dependency (Name, Versions), Success);

      Must_Enter : Boolean;
   begin
      if not Alire.Query.Exists (Name) then
         Trace.Info ("Project [" & Name & "] does not exist in the catalog.");
         raise Command_Failed;
      end if;

      if not Success then
         Trace.Warning ("Failed: could not resolve dependencies.");
         Trace.Warning ("Requested project was " & Name &
                        (if Op /= ASCII.NUL
                           then " with version " & Semver.Image (Versions)
                           else " with most recent version"));
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
            use OS_Lib;
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
