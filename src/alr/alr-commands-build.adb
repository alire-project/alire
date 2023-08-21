with Alire.Builds;
with Alire.Crate_Configuration;
with Alire.Utils.Switches;

with Stopwatch;

package body Alr.Commands.Build is

   Switch_Profiles : constant String := "--profiles";

   --------------------
   -- Apply_Profiles --
   --------------------

   procedure Apply_Profiles (Cmd : in out Command) is
      use Alire.Crate_Configuration;

      Profiles : constant Parsed_Profiles :=
                   Parse_Profiles (Cmd.Profiles.all,
                                   Accept_Wildcards => True);
   begin
      if Cmd.Profiles.all = "" then
         return;
      end if;

      --  Apply wildcard first (only one allowed)

      if Profiles.Default_Apply /= To_None then
         Cmd.Root.Set_Build_Profiles
           (Profiles.Default_Profile,
            Force => Profiles.Default_Apply = To_All);
      end if;

      --  Apply crates last

      declare
         use Profile_Maps;
      begin
         for I in Profiles.Profiles.Iterate loop
            Cmd.Root.Set_Build_Profile (Crate   =>  Key (I),
                                        Profile =>  Element (I));
         end loop;
      end;
   end Apply_Profiles;

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
         Reportaise_Wrong_Arguments ("Only one build profile can be selected");
      end if;

      --  Prevent premature update of dependencies, as the exact folders
      --  will depend on the build hashes, which are yet unknown until
      --  build profiles are applied.
      Cmd.Requires_Workspace (Sync => Alire.Builds.Sandboxed_Dependencies);
      --  For sandboxed dependencies we keep the legacy behavior. We can unify
      --  behaviors when crate configuration is only generated per missing
      --  crate.

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

      --  Effects on all crates first, in case there is root crate override

      Cmd.Apply_Profiles;

      --  Effects on root crate

      if Profiles_Selected /= 0 then -- can only be 1
         Cmd.Root.Set_Build_Profile (Cmd.Root.Name, Profile);
      end if;

      --  And redirect to actual execution procedure

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
      --  Prevent premature update of dependencies, as the exact folders
      --  will depend on the build hashes, which are yet unknown until
      --  build profiles are applied.
      Cmd.Requires_Workspace (Sync => Alire.Builds.Sandboxed_Dependencies);
      --  TODO: remove sync once config generation is per crate.

      declare
         Timer : Stopwatch.Instance;
      begin
         if Cmd.Root.Build (Args,
                            Export_Build_Env,
                            Saved_Profiles => Cmd not in Build.Command'Class)
           --  That is, we apply the saved profiles unless the user is
           --  explicitly invoking `alr build`.
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
       .New_Line
       .Append ("A build profile can be selected with the appropriate switch."
         & " The profile is applied to the root release only, "
         & "whereas dependencies are built in release mode. Use "
         & Switch_Profiles & " for more overrides.")
       .New_Line
       .Append (Switch_Profiles & "="
         & TTY.Emph ("*|%|<crate1>=<profile>[,<crate2>=<profile>...]"))
       .Append ("   Apply profiles to individual crates.")
       .Append ("   Use " & TTY.Emph ("*=<profile>") & " to set all profiles.")
       .Append ("   Use " & TTY.Emph ("%=<profile>") & " to set profiles of "
         & "crates without a setting in a manifest only.")
       .New_Line
       .Append ("Running '" & TTY.Terminal ("alr build") & "' without profile "
         & "switches defaults to development (root crate) + release "
         & " (dependencies). Indirect builds through, e.g., '"
         & TTY.Terminal ("alr run") & "' will use the last '"
         & TTY.Terminal ("alr build") & "' configuration.")
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
                     "Set root build profile to Release");
      Define_Switch (Config,
                     Cmd.Validation_Mode'Access,
                     "", "--validation",
                     "Set root build profile to Validation");
      Define_Switch (Config,
                     Cmd.Dev_Mode'Access,
                     "", "--development",
                     "Set root build profile to Development (default)");

      Define_Switch
        (Config,
         Cmd.Profiles'Access,
         "", Switch_Profiles & "=",
         "Comma-separated list of <crate>=<profile> values (see description)");

   end Setup_Switches;

end Alr.Commands.Build;
