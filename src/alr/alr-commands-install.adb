with Ada.Directories;

with Alire.Containers;
with Alire.Dependencies.Containers;
with Alire.Install;
with Alire.Paths;
with Alire.Platforms.Folders;

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
                           else Alire.Platforms.Folders.Prefix);
      Local_Prefix :  constant Alire.Absolute_Path :=
                        Adirs.Full_Name
                          (if Cmd.Prefix.all /= ""
                           then Cmd.Prefix.all
                           else Alire.Paths
                                     .Prefix_Folder_Inside_Working_Folder);
   begin

      Cmd.Validate;

      if Cmd.Alone then

         Cmd.Requires_Valid_Session;

         if Alire.Install.Root (Local_Prefix).Is_Valid then
            Reportaise_Command_Failed
              ("Installation of unindexed work-in-progress releases in shared "
               & "prefixes is not yet supported.");
         end if;

         --  TODO: uninstall any previous installation at the prefix

         Cmd.Root.Install
           (Prefix     => Local_Prefix,
            With_Root  => True,
            Cmd_Args   => AAA.Strings.Empty_Vector,
            Export_Env => True);

      elsif Args.Is_Empty then

         --  Display info on default/given prefix

         Alire.Install.Print (Global_Prefix);

      elsif Cmd.Delete then

         declare
            Crates : Alire.Containers.Crate_Name_Sets.Set;
         begin
            for Img of Args loop
               Crates.Include (To_Name (Img));
            end loop;

            Alire.Install.Remove (Global_Prefix, Crates);
         end;

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
         & TTY.URL (Alire.Platforms.Folders.Prefix)));

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
                     Cmd.Alone'Access,
                     "", "--standalone",
                     "GPRinstall current workspace only");

      Define_Switch (Config,
                     Cmd.Delete'Access,
                     "", "--del",
                     "Remove release from prefix");

      Define_Switch (Config,
                     Cmd.Prefix'Access,
                     "", "--prefix=",
                     "Override installation prefix (default is "
                     & TTY.URL ("${CRATE_ROOT}/alire/prefix)") & ")");
   end Setup_Switches;

end Alr.Commands.Install;
