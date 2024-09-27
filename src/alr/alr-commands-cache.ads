with AAA.Strings;

package Alr.Commands.Cache is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("cache");

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);
   --  This is called once the command-line is parsed.

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
       .Append ("Inspect and manage Alire's cache.")
       .New_Line
       .Append ("Cache entries can be deleted to reclaim space and will be "
         & "recreated on demand. Beware that deleting toolchains and releases "
         & "may cause potentially large redownloads."));

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration) is null;

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Inspect and manage Alire's cache");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("");

private

   type Command is new Commands.Command with null record;

end Alr.Commands.Cache;
