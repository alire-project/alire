with Ada.Containers;

with Alire; use Alire;
with Alire.Config;
with Alire.OS_Lib.Subprocess;
with Alire.Platforms.Current;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Alr.Commands.Edit is

   ------------------
   -- Start_Editor --
   ------------------

   procedure Start_Editor (Args : in out AAA.Strings.Vector;
                           Prj : Relative_Path)
   is
      Pattern : constant String := "${GPR_FILE}";

      Cmd : constant String := Args.First_Element;

      Replaced_Args : AAA.Strings.Vector;
   begin

      Args.Delete_First;

      for Elt of Args loop

         --  Replace pattern in Elt, if any
         declare
            Us    : Unbounded_String := +Elt;
            Index : Natural;
         begin
            Index := Ada.Strings.Unbounded.Index (Us, Pattern);
            if Index /= 0 then
               Replace_Slice (Us,
                              Low    => Index,
                              High   => Index + Pattern'Length - 1,
                              By     => Prj);
            end if;

            Replaced_Args.Append (+Us);
         end;
      end loop;

      Trace.Info ("Editing crate with: ['" & Cmd & "' '" &
                    AAA.Strings.Flatten (Replaced_Args, "', '") & "']");
      Alire.OS_Lib.Subprocess.Checked_Spawn (Cmd, Replaced_Args);
   end Start_Editor;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      use Ada.Containers;
      use GNAT.Strings;
      use Alire.Config;

      Editor_Cmd  : constant String :=
        Alire.Config.DB.Get (Keys.Editor_Cmd, "gnatstudio -P ${GPR_FILE}");

      Edit_Args : AAA.Strings.Vector := AAA.Strings.Split (Editor_Cmd, ' ');
   begin
      if Args.Count /= 0 then
         Reportaise_Wrong_Arguments (Cmd.Name & " doesn't take arguments");
      end if;

      if Edit_Args.Is_Empty then
         Reportaise_Command_Failed
           ("No editor defined in config key '" & Keys.Editor_Cmd & "'.");
      end if;

      Cmd.Requires_Valid_Session;

      Cmd.Root.Export_Build_Environment;

      declare
         Exec : constant String := Edit_Args.First_Element;
      begin
         if Alire.OS_Lib.Subprocess.Locate_In_Path (Exec) = "" then
            if Exec = "gnatstudio" or else Exec = "gnatstudio.exe" then

               Reportaise_Command_Failed
                 ("GNAT Studio not available or not in PATH. " & ASCII.LF &
                    "You can download it at: " & ASCII.LF &
                    "https://github.com/AdaCore/gnatstudio/releases");
            else
               Reportaise_Command_Failed
                 ("'" & Exec & "' not available or not in PATH.");
            end if;
            return;
         end if;
      end;

      declare
         Project_Files : constant AAA.Strings.Vector :=
           Cmd.Root.Release.Project_Files
             (Platforms.Current.Properties, With_Path => True);
      begin
         if Project_Files.Length = 0 then
            Reportaise_Command_Failed
              ("No project file to open for this crate.");

         elsif Project_Files.Length = 1 then
            Start_Editor (Edit_Args, Project_Files.First_Element);

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
            Start_Editor (Edit_Args, Cmd.Prj.all);
         end if;
      end;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector is
     (AAA.Strings.Empty_Vector
      .Append ("Start GNAT Studio with Alire build environment setup.")
     );

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
                     Cmd.Prj'Access,
                     "", "--project=",
                     "Select the project file to open if the crate " &
                       "provides multiple project files, ignored otherwise");
   end Setup_Switches;

end Alr.Commands.Edit;
