with AAA.Strings;

with CLIC.Subcommand;

with Alire.Toolchains;

package Alr.Commands.Topics.Toolchains is

   type Topic is new CLIC.Subcommand.Help_Topic with null record;

   overriding
   function Name (This : Topic) return CLIC.Subcommand.Identifier
   is ("toolchains");

   overriding
   function Title (This : Topic) return String
   is ("Configuration and use of toolchains");

   overriding
   function Content (This : Topic) return AAA.Strings.Vector
   is (Alire.Toolchains.Description);

end Alr.Commands.Topics.Toolchains;
