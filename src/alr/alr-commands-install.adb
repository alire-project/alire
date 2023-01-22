with Ada.Directories;

with Alire.Dependencies.Containers;
with Alire.Install;

package body Alr.Commands.Install is

   package Adirs renames Ada.Directories;

   --------------
   -- Validate --
   --------------

   procedure Validate (Cmd : Command) is null;
   --  Nothing to validate for now

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      Global_Prefix : constant Alire.Absolute_Path :=
                        Adirs.Full_Name
                          (if Cmd.Prefix.all /= ""
                           then Cmd.Prefix.all
                           else Alire.Install.Default_Prefix);
   begin

      Cmd.Validate;

      if Args.Is_Empty then

         --  Display info on default/given prefix.

         Alire.Install.Info (Global_Prefix);

      else

         --  Install every given dependency

         declare
            Deps : Alire.Dependencies.Containers.List;
         begin
            for Img of Args loop
               Deps.Append (Alire.Dependencies.From_String (Img));
            end loop;

            Alire.Install.Add (Global_Prefix, Deps);
         end;

      end if;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   Binaries : constant String := "gnat, gnatprove, gprbuild, gnatstudio";

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
                     & TTY.URL ("${CRATE_ROOT}/alire/prefix)") & ")");
   end Setup_Switches;

end Alr.Commands.Install;
