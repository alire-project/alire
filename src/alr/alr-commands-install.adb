with Ada.Directories;

--  with Alire.Dependencies;
with Alire.Platforms.Current;

with Stopwatch;

package body Alr.Commands.Install is

   package Adirs renames Ada.Directories;

   --  procedure Install_One (Dep : Alire.Dependencies.Dependency) is null;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
   begin
      if Args.Is_Empty then

         Cmd.Requires_Valid_Session;

         declare
            Timer  : Stopwatch.Instance;
            Prefix : constant Alire.Absolute_Path :=
                       (if Cmd.Prefix.all = ""
                        then Alire.Platforms.Current.Prefix_Folder
                        else Adirs.Full_Name (Cmd.Prefix.all));
         begin
            Cmd.Root.Install
              (Prefix     => Prefix,
               Cmd_Args   => Args,
               Export_Env => True);

            Trace.Info ("Install to " & TTY.URL (Prefix)
                        & " finished successfully in "
                        & TTY.Bold (Timer.Image) & " seconds.");
         end;

      else

         --  Install all given targets

         for Target of Args loop
            Trace.Always ("XXX: " & Target);
            --  Install_One (Alire.Dependencies.From_String (Target));
         end loop;

      end if;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
       .Append ("Invokes gprinstall to install all projects.")
       .Append ("The default install location is "
         & TTY.URL (Alire.Platforms.Current.Prefix_Folder)));

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
                     Cmd.Prefix'Access,
                     "", "--prefix=",
                     "Override installation prefix (default is "
                     & TTY.URL ("${CRATE_ROOT}/alire/prefix)") & ")");
   end Setup_Switches;

end Alr.Commands.Install;
