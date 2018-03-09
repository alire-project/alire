with Ada.Directories;

with Alire.Index;
with Alire.Origins;
with Alire.Releases;

with Alr.Bootstrap;
with Alr.Hardcoded;
with Alr.OS_Lib;
with Alr.Query;
with Alr.Templates;
with Alr.Utils;

with Semantic_Versioning; use Semantic_Versioning;

package body Alr.Commands.Init is

   --------------
   -- Generate --
   --------------

   procedure Generate (Cmd : Command) is
      Name : constant String := Argument (1);
   begin
      if Cmd.In_Place then
         null; -- do nothing
      elsif Cmd.No_Skel then
         Ada.Directories.Create_Directory (Name);
      else
         declare
            use OS_Lib;
         begin
            OS_Lib.Copy_Folder ((if Cmd.Bin
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
         Guard : constant Folder_Guard :=
                   (if Cmd.In_Place
                    then Os_Lib.Stay_In_Current_Folder
                    else OS_Lib.Enter_Folder (Name)) with Unreferenced;

         New_Release : constant Alire.Releases.Release :=
                         Alire.Releases.New_Release
                           (Alire.Projects.Alire_Reserved,
                            V ("0.0.0-working_copy_" & Name),
                            Alire.Origins.New_Filesystem (Ada.Directories.Current_Directory),
                            Notes              => "Working copy of " & Name,
                            Dependencies       => Bootstrap.Alire_Minimal_Dependency,
                            Properties         => Alire.Index.No_Properties,
                            Private_Properties => Alire.Index.No_Properties,
                            Available          => Alire.Index.No_Requisites);
         Success     : Boolean;
         Depends     : constant Query.Instance :=
                         Query.Resolve (New_Release.Depends (Query.Platform_Properties),
                                        Success,
                                        Query_Policy);
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
   begin
      if Num_Arguments /= 1 then
         Trace.Error ("No project name given");
         raise Wrong_Command_Arguments;
      end if;

      if not (Cmd.Bin or Cmd.Lib) then
         Log ("Please provide either --bin or --lib");
         raise Command_Failed;
      end if;

      if Cmd.In_Place then
         Cmd.No_Skel := True;
      end if;

      --  Validation finished

      declare
         Name : constant String := Argument (1);
      begin
         if Utils.To_Lower_Case (Name) = Utils.To_Lower_Case (Templates.Sed_Pattern) then
            Log ("The project name is invalid, as it is used internally by alr; please choose another name");
            raise Command_Failed;
         end if;

         if not Cmd.In_Place and then Ada.Directories.Exists (Name) then
            Log ("Folder " & Utils.Quote (Name) & " already exists, not proceeding.");
            raise Command_Failed;
         end if;

         --  Create and enter folder for generation, if it didn't happen already
         if not Cmd.In_Place and then Session_State >= Outdated then
            if Session_State = Valid and then Name = Root_Release.Project then
               Trace.Info ("Already in working copy, skipping initialization");
            else
               Trace.Error ("Cannot initialize a project inside another alr project, stopping.");
               raise Command_Failed;
            end if;
         else
            Generate (Cmd);
            Log ("Initialization completed");
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
                     Cmd.In_Place'Access,
                     "", "--in-place",
                     "Create alr files in current folder (implies --no-skel)");

      Define_Switch (Config,
                     Cmd.No_Skel'Access,
                     "", "--no-skel",
                     "Do not generate non-alire skeleton files");
   end Setup_Switches;

end Alr.Commands.Init;
