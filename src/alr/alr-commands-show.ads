with AAA.Strings;

package Alr.Commands.Show is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("show");

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector;

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding function Short_Description (Cmd : Command) return String is
     ("See information about a release");

   overriding function Usage_Custom_Parameters (Cmd : Command) return String is
     ("[<crate>[allowed versions]] [--system] [--external[-detect]]"
      & " | --graph | --jekyll | --solve | --tree");

private

   procedure Validate (Cmd  : in out Command;
                       Args : AAA.Strings.Vector);

   type Command is new Commands.Command with record
      Dependents : aliased Boolean := False;
      Detail     : aliased Boolean := False;
      Detect     : aliased Boolean := False;
      External   : aliased Boolean := False;
      Graph      : aliased Boolean := False;
      Solve      : aliased Boolean := False;
      System     : aliased Boolean := False;
      Tree       : aliased Boolean := False;
      Jekyll     : aliased Boolean := False;
   end record;

end Alr.Commands.Show;
