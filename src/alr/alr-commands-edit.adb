with Ada.Containers;

with Alire; use Alire;
with Alire.Formatting;
with Alire.Settings.Builtins;
with Alire.Spawn;
with Alire.OS_Lib.Subprocess;
with Alire.Platforms.Current;

with CLIC.User_Input;

package body Alr.Commands.Edit is

   package Format renames Alire.Formatting;

   Switch_Select : constant String := "--select-editor";

   --------------------
   -- Set_Config_Cmd --
   --------------------

   procedure Set_Config_Cmd (Cmd : String) is
      use Trace;
   begin
      if Cmd /= "" then
         Settings.Set_Globally (Settings.Builtins.Editor_Cmd, Cmd);
         Put_Info ("'" & TTY.Terminal (Cmd)
                   & "' is now set as the editor command.");
      else
         Settings.Builtins.Editor_Cmd.Unset (Alire.Settings.Global);
         Put_Info ("The editor command has been unset.");
      end if;

      Put_Info ("You can change editors by running the following command:");
      Put_Info ("`alr edit " & Switch_Select & "`");
   end Set_Config_Cmd;

   -----------------------------
   -- Report_If_Not_Installed --
   -----------------------------

   procedure Report_If_Not_Installed (Exec : String) is
   begin
      if Alire.OS_Lib.Subprocess.Locate_In_Path (Exec) = "" then
         --  Executable not in PATH, report an error
         if Exec = "gnatstudio" or else Exec = "gnatstudio.exe" then
            Reportaise_Command_Failed
              ("GNAT Studio not available or not in PATH. " & ASCII.LF &
                 "You can download it at: " & ASCII.LF &
                 "https://github.com/AdaCore/gnatstudio/releases");
         elsif Exec = "code" or else Exec = "code.exe" then
            Reportaise_Command_Failed
              ("VS Code not available or not in PATH. " & ASCII.LF &
                 "You can download it at: " & ASCII.LF &
                 "https://code.visualstudio.com/download"  & ASCII.LF &
                 "We also recomend installing the 'AdaCore.ada' extension.");
         else
            Reportaise_Command_Failed
              ("'" & Exec & "' not available or not in PATH.");
         end if;
      end if;
   end Report_If_Not_Installed;

   ------------------
   -- Query_Editor --
   ------------------

   procedure Query_Editor is
      use AAA.Strings;
      use CLIC.User_Input;

      package Builtins renames Alire.Settings.Builtins;

      type Editor_Choice is (VScode, GNATstudio, Other);

      subtype Editor_With_Command
        is Editor_Choice range VScode .. GNATstudio;

      function Img (E : Editor_Choice) return String
      is (case E is
             when VScode     => "VS Code",
             when GNATstudio => "GNAT studio",
             when Other      => "Other");

      function Cmd (E : Editor_With_Command) return String
      is (case E is
             when VScode     =>
                "code " & Format.Dollar_Image (Format.Crate_Root),
             when GNATstudio =>
                "gnatstudio -P " & Format.Dollar_Image (Format.GPR_File));
      Choices : AAA.Strings.Vector;

   begin

      if Builtins.Editor_Cmd.Is_Empty then
         Put_Info ("There is no editor currently configured.");
      else
         Put_Info ("The current editor command is: '"
                   & TTY.Terminal (Builtins.Editor_Cmd.Get) & "'");
      end if;

      for Ed in Editor_Choice loop
         Choices.Append (Img (Ed));
      end loop;

      declare
         Answer_Index : constant Positive :=
           Query_Multi ("Please select your prefered editor" &
                          " or 'other' to enter a custom command",
                        Choices);

         Answer : constant Editor_Choice :=
           Editor_Choice'Val
             (Editor_Choice'Pos (Editor_Choice'First) + (Answer_Index - 1));

      begin
         case Answer is
            when Editor_With_Command =>
               Set_Config_Cmd (Cmd (Answer));

            when Other  =>
               Trace.Always
                 ("In your custom editor command, `alr` will replace "
                  & TTY.Emph (Format.Dollar_Image (Format.Crate_Root))
                  & " and " & TTY.Emph (Format.Dollar_Image (Format.GPR_File))
                  & " patterns with the workspace root or project file path, "
                  & "respectively.");
               declare
                  Custom : constant String :=
                    Query_String ("Please enter a custom editor command",
                                  "", null);
               begin
                  Set_Config_Cmd (Custom);
               end;
         end case;
      end;
   end Query_Editor;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      use Ada.Containers;
      use GNAT.Strings;
      use Alire.Settings;

      package Builtins renames Alire.Settings.Builtins;
   begin
      if Args.Count /= 0 then
         Reportaise_Wrong_Arguments (Cmd.Name & " doesn't take arguments");
      end if;

      --  Launch the editor selection if explicitly asked for
      if Cmd.Set then
         Query_Editor;
         return;
      end if;

      --  Check if editor command is defined in the configuration
      if Builtins.Editor_Cmd.Is_Empty then
         Query_Editor;
      end if;

      --  Check again after asking user for prefered editor
      if Builtins.Editor_Cmd.Is_Empty then
         Reportaise_Command_Failed
           ("No editor defined in config key '"
            & Builtins.Editor_Cmd.Key & "'.");
      end if;

      Cmd.Requires_Workspace;

      Cmd.Root.Export_Build_Environment;

      declare
         Project_Files : constant AAA.Strings.Vector :=
           Cmd.Root.Release.Project_Files
             (Platforms.Current.Properties, With_Path => True);
      begin
         if Project_Files.Length = 0 then
            Reportaise_Command_Failed
              ("No project file to open for this crate.");

         elsif Project_Files.Length = 1 then
            Spawn.Settings_Command
              (Builtins.Editor_Cmd.Get,
               Format.For_Editor (Cmd.Root, Project_Files.First_Element),
               Report_If_Not_Installed'Access);

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
            Spawn.Settings_Command
              (Builtins.Editor_Cmd.Get,
               Format.For_Editor (Cmd.Root, Cmd.Prj.all),
               Report_If_Not_Installed'Access);
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
      .Append ("Start an editor with Alire build environment setup.")
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
                       "provides multiple project files, ignored otherwise",
                     Argument => "FILE");
      Define_Switch (Config,
                     Cmd.Set'Access,
                     "", "--select-editor",
                     "Launch the interactive editor selector");
   end Setup_Switches;

end Alr.Commands.Edit;
