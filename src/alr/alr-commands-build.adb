with Stopwatch;
with Alire.Utils;
with Alire.Utils.Switches;
with Alire.Crate_Configuration;

package body Alr.Commands.Build is

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

      if Cmd.Recurse_Unset and then Cmd.Recurse_Force then
         Reportaise_Wrong_Arguments
           ("Only one recursive mode can be selected");
      end if;

      if (Cmd.Recurse_Unset or else Cmd.Recurse_Force)
        and then Profiles_Selected = 0
      then
         Reportaise_Wrong_Arguments
           ("Must specify a build profile with a recursive profile option");
      end if;

      --  Build profile in the command line takes precedence. The configuration
      --  will have been loaded at this time with all profiles found in
      --  manifests.

      if Cmd.Release_Mode then
         Profile := Release;
         Cmd.Root.Set_Build_Profile (Cmd.Root.Name, Release);
      elsif Cmd.Validation_Mode then
         Profile := Validation;
         Cmd.Root.Set_Build_Profile (Cmd.Root.Name, Validation);
      elsif Cmd.Dev_Mode then
         Profile := Development;
         Cmd.Root.Set_Build_Profile (Cmd.Root.Name, Development);
      end if;

      if Profiles_Selected /= 0 then -- can only be 1
         Cmd.Root.Set_Build_Profile (Cmd.Root.Name, Profile);
      end if;

      if Cmd.Recurse_Unset or else Cmd.Recurse_Force then
         Cmd.Root.Set_Build_Profiles (Profile, Force => Cmd.Recurse_Force);
         Cmd.Root.Generate_Configuration;
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

      --  If we were invoked from another command (e.g. run) we apply the
      --  last profile used for building the crate

      if Cmd not in Command'Class then
         Cmd.Root.Set_Build_Profile
           (Cmd.Root.Name,
            Alire.Crate_Configuration.Last_Build_Profile);
      end if;

      declare
         Timer : Stopwatch.Instance;
      begin
         if Cmd.Root.Build (Args, Export_Build_Env) then

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
         & " crate."));

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
                     "Set root crate build mode to Release");
      Define_Switch (Config,
                     Cmd.Validation_Mode'Access,
                     "", "--validation",
                     "Set root crate build mode to Validation");
      Define_Switch (Config,
                     Cmd.Dev_Mode'Access,
                     "", "--development",
                     "Set root crate build mode to Development (default)");

      Define_Switch (Config,
                     Cmd.Recurse_Unset'Access,
                     "", "--recurse-unset",
                     "Set build mode also for dependencies without "
                     & "an explicit setting");
      Define_Switch (Config,
                     Cmd.Recurse_Force'Access,
                     "", "--recurse-all",
                     "Set build mode also for all dependencies");

   end Setup_Switches;

end Alr.Commands.Build;
