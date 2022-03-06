with Ada.Directories;

with Alire.Dependencies.Containers;
with Alire.Install;

package body Alr.Commands.Install is

   package Adirs renames Ada.Directories;

   --------------
   -- Validate --
   --------------

   procedure Validate (Cmd : Command) is null;
   --  TODO: implement switches validation

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

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
       .Append ("Manages installations of crates to a common prefix.")
       .Append ("The default install location is "
         & TTY.URL (Alire.Install.Default_Prefix)));

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
