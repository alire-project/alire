with Stopwatch;
with Alire.Utils;
with Alire.Utils.Switches;

package body Alr.Commands.Build is

   Apply_Switch : constant String := "--apply-profile";

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      use Alire.Utils.Switches;
      Profiles_Selected : constant Natural :=
                            Alire.Utils.Count_True ((Cmd.Release_Mode,
                                                     Cmd.Validation_Mode,
                                                    Cmd.Dev_Mode));
      Profile : Profile_Kind;
   begin
      if Profiles_Selected > 1 then
         Reportaise_Wrong_Arguments ("Only one build mode can be selected");
      end if;

      if Cmd.Apply_Profile.all /= Apply_Default
        and then Profiles_Selected = 0
      then
         Reportaise_Wrong_Arguments
           ("Must specify a build profile when using "
            & TTY.Terminal (Apply_Switch));
      end if;

      if (for all Mode of Apply_Modes =>
            Mode.all /= Cmd.Apply_Profile.all)
      then
         Reportaise_Command_Failed ("Invalid value for " & Apply_Switch & ": "
                                    & TTY.Error (Cmd.Apply_Profile.all));
      end if;

      --  Build profile in the command line takes precedence. The configuration
      --  will have been loaded at this time with all profiles found in
      --  manifests.

      if Cmd.Release_Mode then
         Profile := Release;
      elsif Cmd.Validation_Mode then
         Profile := Validation;
      elsif Cmd.Dev_Mode then
         Profile := Development;
      end if;

      --  Effects on root crate

      if Profiles_Selected /= 0 then -- can only be 1
         Cmd.Root.Set_Build_Profile (Cmd.Root.Name, Profile);
      end if;

      --  Effects on dependencies

      if Cmd.Apply_Profile.all /= Apply_Default and then
         Cmd.Apply_Profile.all /= Apply_Root
      then
         Cmd.Root.Set_Build_Profiles
           (Profile,
            Force => Cmd.Apply_Profile.all = Apply_All);
      end if;

      if not Execute (Cmd, Args,
                      Export_Build_Env => True)
      then
         Reportaise_Command_Failed ("Compilation failed.");
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (Cmd              : in out Commands.Command'Class;
                     Args             :        AAA.Strings.Vector;
                     Export_Build_Env :        Boolean)
                     return Boolean
   is
   begin

      declare
         Timer : Stopwatch.Instance;
      begin
         if Cmd.Root.Build (Args,
                            Export_Build_Env,
                            Saved_Profiles => Cmd not in Build.Command'Class)
         then

            Trace.Info ("Build finished successfully in "
                        & TTY.Bold (Timer.Image) & " seconds.");
            Trace.Detail ("Use alr run --list to check available executables");

            return True;

         else
            return False;
         end if;
      end;

   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
       .Append ("Invokes gprbuild to compile all targets in the current"
         & " crate.")
       .Append ("")
       .Append ("A build profile can be selected with the appropriate switch."
         & " By default the profile is applied to the root release only, "
         & "whereas dependencies are built in release mode. Use "
         & Apply_Switch & " to alter this behavior.")
       .Append ("")
       .Append (Apply_Switch & "=" & Apply_Root
         & " (default): apply given profile to root only.")
       .Append (Apply_Switch & "=" & Apply_Unset
         & ": apply profile to all releases without an "
         & "explicit setting in some manifest.")
       .Append (Apply_Switch & "=" & Apply_All
         & ": apply profile to all releases unconditionally.")
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
                     Cmd.Release_Mode'Access,
                     "", "--release",
                     "Set build profile to Release");
      Define_Switch (Config,
                     Cmd.Validation_Mode'Access,
                     "", "--validation",
                     "Set build profile to Validation");
      Define_Switch (Config,
                     Cmd.Dev_Mode'Access,
                     "", "--development",
                     "Set build profile to Development (default)");

      Define_Switch (Config,
                     Cmd.Apply_Profile'Access,
                     "", "--apply-profile=",
                     "Set build profile to one of root (default), unset, all");

   end Setup_Switches;

end Alr.Commands.Build;
