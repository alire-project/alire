with Alire.Config.Internal;
with Alire.Shared;
with Alire.Utils.TTY;

with CLIC.Config.Info;

package body Alr.Commands.Share is

   ------------
   -- Modify --
   ------------

   procedure Modify (Cmd   : in out Command;
                     Crate : String)
   is
      use all type Alire.Shared.Explicit_Requests;

      --  --yes and --local are defaults, resulting in this sequence:
      Status : constant Alire.Shared.Explicit_Requests :=
                 (if Cmd.Reset then
                    (if Cmd.Global
                     then Reset_Global
                     else Reset_Local)
                  elsif Cmd.No and then Cmd.Global then
                     No_Global
                  elsif Cmd.Global then
                     Yes_Global
                  elsif Cmd.No then
                     No_Local
                  else
                     Yes_Local);
   begin
      if Status in Yes_Local | No_Local | Reset_Local then
         Cmd.Requires_Workspace;
      end if;

      Alire.Shared.Mark (+Crate, Status);

      Alire.Put_Success
        ("Crate " & Alire.Utils.TTY.Name (Crate)
         & " marked as " & TTY.Emph (Alire.Shared.Image (Status)));
   end Modify;

   --------------
   -- Validate --
   --------------

   procedure Validate (Cmd  : in out Command;
                       Args :        AAA.Strings.Vector)
   is
   begin
      if Cmd.Local and then Cmd.Global then
         Reportaise_Wrong_Arguments
           ("--local and --global are incompatible");
      end if;

      if Cmd.Yes and then Cmd.No then
         Reportaise_Wrong_Arguments
           ("--yes and --no are incompatible");
      end if;

      if Cmd.Reset and then
        (Cmd.Yes or Cmd.No or Cmd.List)
      then
         Reportaise_Wrong_Arguments
           ("--reset is incompatible with other switches");
      end if;

      if Cmd.List and then not Args.Is_Empty then
         Reportaise_Wrong_Arguments
           ("--list is incompatible with crate names");
      end if;

      if Cmd.List and then
        (Cmd.Yes or Cmd.No or Cmd.Local or Cmd.Global or Cmd.Reset)
      then
         Reportaise_Wrong_Arguments
           ("--list is incompatible with other switches");
      end if;

      if (Cmd.Yes or Cmd.No or Cmd.Local or Cmd.Global) and then Args.Is_Empty
      then
         Reportaise_Wrong_Arguments
           ("Sharing hints require at least one crate name");
      end if;
   end Validate;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
   begin
      Validate (Cmd, Args);

      if Cmd.List or else Args.Is_Empty then
         Trace.Always
           (CLIC.Config.Info.List
              (Alire.Config.DB,
               Filter      =>
                 ".*"
               & AAA.Strings.To_Lower_Case
                 (Alire.Config.Internal.Shared_Dependencies'Image)
               & ".*",
               Show_Origin => False).Flatten (ASCII.LF));

      elsif not Args.Is_Empty then
         for Crate of Args loop
            Cmd.Modify (Crate);
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
       .Append
         ("Dependencies that share storage are not deployed to the usual"
          & " workspace cache location but to the configuration-wide cache "
          & "directory. This may be useful for large dependencies with a slow "
          & "build process. Note that these dependencies may still be rebuilt "
          & "when a different build profile is requested.")
       .New_Line
       .Append
         ("Some dependencies are intrinsically shared and cannot be unshared: "
          & "binary crates, system packages, and environment executables.")
       .New_Line
       .Append
         ("Sharing requests take precedence over the '"
          & Alire.Config.Keys.Dependencies_Dir & "' configuration key value.")
       .New_Line
       .Append
         ("As with configuration keys, the local workspace configuration takes"
          & " precedence over the global configuration.")
       .New_Line
       .Append
         ("--yes (default) requests the sharing of the given crate, whereas "
          & "--no will prevent sharing. --local (default) will store the "
          & "sharing request in the workspace, whereas --global will use the "
          & "configuration-wide settings.")
       .New_Line
       .Append
         ("Use --reset to remove sharing requests for a given crate from the "
          & "--local (default) or --global configurations.")
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
      Define_Switch
        (Config      => Config,
         Output      => Cmd.List'Access,
         Long_Switch => "--list",
         Help        => "List sharing hints");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Local'Access,
         Long_Switch => "--local",
         Help        => "Store sharing hint in local workspace (default)");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Global'Access,
         Long_Switch => "--global",
         Help        => "Store sharing hint global configuration");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Yes'Access,
         Long_Switch => "--yes",
         Help        => "Request sharing of given crate(s) (default)");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.No'Access,
         Long_Switch => "--no",
         Help        => "Forbid sharing of given crate(s)");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Reset'Access,
         Long_Switch => "--reset",
         Help        => "Remove sharing hints of given crate(s)");
   end Setup_Switches;

end Alr.Commands.Share;
