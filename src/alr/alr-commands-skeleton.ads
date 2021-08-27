with AAA.Strings;

package Alr.Commands.Skeleton is

   --  Empty command that you can rename to provide a new command. See also
   --  subprogram documentation in Alr.Commands spec. You need also to add its
   --  entry in the Alr.Commands.Dispatch_Table (in the body).

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return String
   is ("skeleton");

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector) is null;
   --  This is called once the command-line is parsed.

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector
   is (AAA.Strings.Empty_Vector
       .Append ("Replace this description with yours.")
       .Append ("Every single line will be reformatted into 79-column-wide"
                & " paragraphs.")
       .New_Line
       .Append ("You can use empty lines for structure with New_Line"));

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommander.Switches_Configuration) is null;

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Your one-liner description of the command");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("Parameters expected after the command name");

private

   type Command is new Commands.Command with null record;

end Alr.Commands.Skeleton;
