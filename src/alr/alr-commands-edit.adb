with Ada.Containers;

with Alire; use Alire;
with Alire.Utils; use Alire.Utils;
with Alire.OS_Lib.Subprocess;

with Alr.Platform;
with Alr.Root;

package body Alr.Commands.Edit is

   ----------------------
   -- Start_GNATstudio --
   ----------------------

   procedure Start_GNATstudio (Prj : Relative_Path) is
   begin
      Alire.OS_Lib.Subprocess.Checked_Spawn
        ("gnatstudio", Empty_Vector & "-P" & Prj);
   end Start_GNATstudio;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      use Ada.Containers;
      use GNAT.Strings;
   begin
      Requires_Full_Index;

      Requires_Valid_Session;

      Alr.Root.Current.Export_Build_Environment;

      if Alire.OS_Lib.Subprocess.Locate_In_Path ("gnatstudio") = "" then
         Reportaise_Command_Failed
           ("GNATstudio not available or not in PATH. " & ASCII.LF &
              "You can download the Community edition at: " & ASCII.LF &
              "https://www.adacore.com/download");
         return;
      end if;

      declare
         Project_Files : constant Alire.Utils.String_Vector :=
           Root.Current.Release.Project_Files
             (Platform.Properties, With_Path => True);
      begin
         if Project_Files.Length = 0 then
            Reportaise_Command_Failed
              ("No project file to open for this crate.");

         elsif Project_Files.Length = 1 then
            Start_GNATstudio (Project_Files.First_Element);

         elsif Cmd.Prj = null
           or else
             not Project_Files.Contains (Cmd.Prj.all)
         then
            Trace.Warning ("More than 1 project file for this crate.");
            Trace.Warning ("The list of project is:");
            for Prj of Project_Files loop
               Trace.Warning (" - " & Prj);
            end loop;
            Reportaise_Command_Failed
              ("Please specify a project file with --project=.");

         else
            Start_GNATstudio (Cmd.Prj.all);
         end if;
      end;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector is
     (Alire.Utils.Empty_Vector
      .Append ("Start GNATstudio with Alire build environment setup.")
     );

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
                     Cmd.Prj'Access,
                     "", "--project=",
                     "Select the project file to open if the crate " &
                       "provides multiple project files, ignored otherwise");
   end Setup_Switches;

end Alr.Commands.Edit;
