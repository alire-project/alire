with AAA.Strings;

private with GNAT.Strings;

package Alr.Commands.Exec is

   type Command is new Commands.Command with private;

   overriding
   function Name (Cmd : Command) return CLIC.Subcommand.Identifier
   is ("exec");

   overriding
   function Switch_Parsing (This : Command)
                            return CLIC.Subcommand.Switch_Parsing_Kind
   is (CLIC.Subcommand.Before_Double_Dash);

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector);

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
       .Append ("Alr sets up the environment variables (GPR_PROJECT_PATH, ")
       .Append ("PATH, etc.) and then spawns the given command.")
       .New_Line
       .Append ("This can be used to run tools or scripts on Alire projects.")
       .New_Line
       .Append ("The ""-P"" switch can be used to ask Alire to insert a ")
       .Append ("""" & Formatter.Terminal ("-P <PROJECT_FILE>")
                  & """ switch to the command arguments.")
       .Append ("""-P"" takes an optional position argument to specify where")
       .Append ("to insert the extra switch. ""-P1"" means first position, ")
       .Append ("""-P2"" second position, etc. ""-P-1"" means last position, ")
       .Append ("""-P-2"" penultimate position, etc. ""-P"" equals ""-P1"".")
       .Append ("For example, """
                & Formatter.Terminal ("alr exec -P2 -- python3 main.py arg1")
                & """ will")
       .Append ("run the following command:")
       .Append ("[""python3"", ""main.py"", ""-P"", ""crate.gpr"", ""arg1""]")
      );

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding
   function Short_Description (Cmd : Command) return String
   is ("Run the given command in the alire project context");

   overriding
   function Usage_Custom_Parameters (Cmd : Command) return String
   is ("[-P?] [--] <executable/script> [<switches and arguments>]");

private

   NO_PROJECT_STR : constant String := "NO_PROJECT";
   --  There is no way to see the difference between a -P switch without
   --  argument and no -P switch at all. So we set this default value to
   --  detect if it is changed by the command line parsing.

   type Command is new Commands.Command with record
      Prj : aliased GNAT.Strings.String_Access := new String'(NO_PROJECT_STR);
   end record;

end Alr.Commands.Exec;
