with Ada.Containers;

with Alr.Platform;

with Alire.Spawn;
with Alire.Utils;

package body Alr.Commands.Coverage is

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      use AAA.Strings;

      GNATcov_Args : AAA.Strings.Vector;
   begin

      --  Check no multi-action
      case Alire.Utils.Count_True
        ((Cmd.Instrument,
          Cmd.Report))
      is
         when 0 =>
            Reportaise_Wrong_Arguments
              ("Specify at least one coverage subcommand");
         when 1 =>
            null; -- Usual case, just fall through
         when others =>
            Reportaise_Wrong_Arguments
              ("Specify exactly one coverage subcommand");
      end case;

      if Cmd.Instrument then
         GNATcov_Args.Append ("instrument");
      elsif Cmd.Report then
         GNATcov_Args.Append ("coverage");
      end if;

      GNATcov_Args.Append ("-P");

      declare
         use Ada.Containers;
         use GNAT.Strings;

         Project_Files : constant AAA.Strings.Vector :=
           Cmd.Root.Release.Project_Files
             (Platform.Properties, With_Path => True);
      begin
         if Project_Files.Length = 0 then
            Reportaise_Command_Failed
              ("No project file to open for this crate.");

         elsif Project_Files.Length = 1 then
            GNATcov_Args.Append (Project_Files.First_Element);

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
            GNATcov_Args.Append (Cmd.Prj.all);
         end if;
      end;

      GNATcov_Args.Append (Args);

      declare
         Cmd_Name : constant String := "gnatcov";
      begin
         Cmd.Requires_Valid_Session;
         Cmd.Root.Export_Build_Environment;

         Alire.Spawn.Command (Cmd                 => Cmd_Name,
                              Args                => GNATcov_Args,
                              Understands_Verbose => False);
      end;
   end Execute;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
   begin
      Define_Switch (Config,
                     Cmd.Instrument'Access,
                     Long_Switch => "--instrument",
                     Help        => "Instrument code for coverage analysis");

      Define_Switch (Config,
                     Cmd.Report'Access,
                     Long_Switch => "--report",
                     Help        => "Produce coverage analysis report");

      Define_Switch (Config,
                     Cmd.Prj'Access,
                     "", "--project=",
                     "Select the project file to open if the crate " &
                       "provides multiple project files, ignored otherwise");
   end Setup_Switches;

end Alr.Commands.Coverage;
