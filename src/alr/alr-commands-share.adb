with Alire.Config.Internal;

with CLIC.Config.Info;

package body Alr.Commands.Share is

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
        (Cmd.Yes or Cmd.No or Cmd.Local or Cmd.Global or Cmd.List)
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
      end if;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector);

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
