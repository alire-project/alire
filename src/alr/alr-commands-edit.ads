with AAA.Strings;

private with GNAT.Strings;

package Alr.Commands.Edit is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return String
   is ("edit");

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
   is ("Start GNATstudio with Alire build environment setup");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("");

private

   type Command is new Commands.Command with record
      Prj : aliased GNAT.Strings.String_Access;
   end record;
end Alr.Commands.Edit;
