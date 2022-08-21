with AAA.Strings;

private with GNAT.OS_Lib;

package Alr.Commands.Build is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("build");

   overriding
   function Switch_Parsing (This : Command)
                            return CLIC.Subcommand.Switch_Parsing_Kind
   is (CLIC.Subcommand.Before_Double_Dash);
   --  For the build command we want the args after -- to pass them to gprbuild

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);

   function Execute (Cmd              : in out Commands.Command'Class;
                     Args             :        AAA.Strings.Vector;
                     Export_Build_Env :        Boolean)
                     return Boolean;
   --  Returns True if compilation succeeded. For invocations after some other
   --  command that already has set up the build environment we need to avoid
   --  redoing it, or it results in "variable already set" errors.

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector;

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("GPRbuild current working release");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[--] [gprbuild switches and arguments]");

private

   Apply_Default : aliased String := "default"; -- To detect explicit setting
   Apply_Root    : aliased String := "root";    -- Apply only to root release
   Apply_Unset   : aliased String := "unset";   -- Apply to releases without
   --  a setting in a manifest
   Apply_All     : aliased String := "all";     -- Apply to all releases

   Apply_Modes : constant array (Positive range <>)
     of GNAT.OS_Lib.String_Access := (Apply_Default'Access,
                                      Apply_Root'Access,
                                      Apply_Unset'Access,
                                      Apply_All'Access);

   type Command is new Commands.Command with record
      Release_Mode    : aliased Boolean := False;
      Validation_Mode : aliased Boolean := False;
      Dev_Mode        : aliased Boolean := False;
      Apply_Profile   : aliased GNAT.OS_Lib.String_Access :=
                          new String'(Apply_Default); -- One of the above
   end record;
end Alr.Commands.Build;
