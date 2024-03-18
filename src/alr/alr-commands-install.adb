with Ada.Directories;

with Alire.Dependencies.Containers;
with Alire.Install;

with Stopwatch;

package body Alr.Commands.Install is

   package Adirs renames Ada.Directories;

   --------------
   -- Validate --
   --------------

   procedure Validate (Cmd : in out Command; Args : AAA.Strings.Vector) is
   begin
      --  If nothing given, we must be in workspace
      if not Cmd.Info and then Args.Is_Empty then
         Cmd.Requires_Workspace
           (Error => "Give a crate name to install or enter a local crate");
      end if;

      if Cmd.Info and then not Args.Is_Empty then
         Reportaise_Wrong_Arguments
           ("You cannot request information and "
            & "install crates simultaenously");
      end if;
   end Validate;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      use all type Alire.Install.Actions;

      Prefix : constant Alire.Absolute_Path :=
                 Adirs.Full_Name
                   (if Cmd.Prefix.all /= ""
                    then Cmd.Prefix.all
                    else Alire.Install.Default_Prefix);

      Timer : Stopwatch.Instance;
   begin
      Cmd.Validate (Args);

      if Cmd.Info then

         --  Display info on default/given prefix.

         Alire.Install.Info (Prefix);

      else

         Cmd.Auto_Update_Index;

         if Args.Is_Empty then

            case Alire.Install.Check_Conflicts (Prefix, Cmd.Root.Release) is
            when Skip =>
               Trace.Info
                 (Cmd.Root.Release.Milestone.TTY_Image
                  & " is already installed, use " & TTY.Terminal ("--force")
                  & " to reinstall");
            when New_Install | Reinstall | Replace =>
               Cmd.Root.Install (Prefix     => Prefix);
            end case;

         else

            --  Install every given dependency

            declare
               Deps : Alire.Dependencies.Containers.List;
            begin
               for Img of Args loop
                  Deps.Append (Alire.Dependencies.From_String (Img));
               end loop;

               Alire.Install.Add (Prefix, Deps);
            end;

            Alire.Put_Success ("Install to " & TTY.URL (Prefix)
                               & " finished successfully in "
                               & TTY.Bold (Timer.Image) & " seconds.");

         end if;
      end if;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   Binaries : constant String := "gnat, gnatprove, gprbuild";

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
       .Append ("Manages installations of releases to a common prefix.")
       .Append ("The default install location is "
         & TTY.URL (Alire.Install.Default_Prefix))
       .New_Line
       .Append ("Installation prefixes are intended to make binaries or "
         & "dynamic libraries available outside of the Alire environment, "
         & "normally by adding the " & TTY.URL ("<prefix>/bin")
         &  " folder to the user's path.")
       .New_Line
       .Append ("Although Alire will vet trivially detectable conflicts "
         & "(e.g., trying to install two executable release with different "
         & "versions), Alire is not aware of the exact binary artifacts "
         & "produced by compiled crates. Thus, you are ""on your own"" in "
         & "regard to the final consistency of installations.")
       .New_Line
       .Append ("That said, binary crates from the Alire project (" & Binaries
         & "), as well as crates initialized with `alr` using default "
         & "templates, should be able to coexist in a same installation prefix"
         & " without issue.")
       .New_Line
       .Append ("You can use the --force global option to reinstall "
         & "already installed releases.")
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
                     Cmd.Prefix'Access,
                     "", "--prefix=",
                     "Override installation prefix (default is "
                     & TTY.URL (Alire.Install.Default_Prefix) & ")",
                     Argument => "DIR");

      Define_Switch (Config,
                     Cmd.Info'Access,
                     "", "--info",
                     "Show info about a installation prefix");
   end Setup_Switches;

end Alr.Commands.Install;
