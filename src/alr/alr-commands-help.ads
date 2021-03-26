package Alr.Commands.Help is

   --  Help command. While commands provide their own help text, other topics
   --  can be defined here.

   type Command is new Commands.Command with private;

   procedure Display_Help (Keyword : String);
   --  Display the help for this keyword, be it command or topic.

   procedure Display_Valid_Keywords;
   --  List a summary of both commands and other topics.

   procedure Display_Valid_Topics;
   --  List a summary of topics which are not commands.

   overriding
   procedure Execute (Cmd : in out Command);

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector
   is (Alire.Utils.Empty_Vector
       .Append ("Shows information about commands and topics.")
       .Append ("See available commands with 'alr help commands'")
       .Append ("See available topics with 'alr help topics'."));

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration) is null;

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Shows help on the given command/topic");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[command|topic]");

   function Highlight_Switches (Line : String) return String;
   --  Visit every word and apply Emph to words starting with --

private

   type Command is new Commands.Command with null record;

end Alr.Commands.Help;
