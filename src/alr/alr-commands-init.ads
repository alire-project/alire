with AAA.Strings;

package Alr.Commands.Init is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return SubCommander.Identifier
   is ("init");

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector;

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Creates a new working release with"
       & " alire metadata, or generate metadata");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("{--bin|--lib} <crate name>");

private

   type Command is new Commands.Command with record
      Bin,
      Lib,
      In_Place,
      No_Skel : aliased Boolean := False;
   end record;

end Alr.Commands.Init;
