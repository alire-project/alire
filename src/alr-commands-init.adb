with Ada.Directories;

with Alire.Origins;
with Alire.Releases;
with Alire.Roots;

with Alr.Hardcoded;
with Alr.OS_Lib;
with Alr.Parsers;
with Alr.Platform;
with Alr.Templates;
with Alr.Utils;

package body Alr.Commands.Init is

   --------------
   -- Generate --
   --------------

   procedure Generate (Cmd : Command) is
      Name  : constant String := Argument (1);
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

         New_Root : constant Alire.Roots.Root := Alire.Roots.New_Root (+Name);
      begin
         OS_Lib.Create_Folder (Hardcoded.Alr_Working_Folder);

         Templates.Generate_Prj_Alr
           (Templates.Unreleased,
            +Name,
            Deps => New_Root.Dependencies.Evaluate (Platform.Properties));

         Templates.Generate_Agg_Gpr (New_Root);
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
         Check : constant Parsers.Allowed_Milestones :=
                   Parsers.Project_Versions (Name)
                   with Unreferenced;
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
         if not Cmd.In_Place and then Session_State >= Detached then
            if Session_State = Valid and then Name = +Root.Project then
               Trace.Info ("Already in working copy, skipping initialization");
            else
               Trace.Error ("Cannot initialize a project inside another alr project, stopping.");
               raise Command_Failed;
            end if;
         else
            Generate (Cmd);
            Trace.Detail ("Initialization completed");
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
