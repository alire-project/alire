with AAA.Strings;

with CLIC.Subcommand;

package Alr.Commands.Topics.Aliases is

   type Topic is new CLIC.Subcommand.Help_Topic with null record;

   overriding
   function Name (This : Topic) return CLIC.Subcommand.Identifier
   is ("aliases");

   overriding
   function Title (This : Topic) return String
   is ("User defined command aliases");

   overriding
   function Content (This : Topic) return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
       .Append ("Command aliases can be defined in local or global ")
       .Append ("configuration.")
       .New_Line
       .Append ("For example the following command:")
       .Append ("""$ alr config --set --global alias.graph 'show --graph'""")
       .Append ("Defines a global alias for the 'show' command with a ")
       .Append ("'--graph' switch.")
       .New_Line
       .Append ("""$ alr graph"" is equivalent to ""alr show --graph"""));

end Alr.Commands.Topics.Aliases;
