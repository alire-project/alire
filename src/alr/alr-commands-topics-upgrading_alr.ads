with AAA.Strings;

with CLIC.Subcommand;

package Alr.Commands.Topics.Upgrading_Alr is

   type Topic is new CLIC.Subcommand.Help_Topic with null record;

   overriding
   function Name (This : Topic) return CLIC.Subcommand.Identifier
   is ("upgrading_alr");

   overriding
   function Title (This : Topic) return String
   is ("Notes on upgrading between major `alr` versions");

   overriding
   function Content (This : Topic) return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
       .Append ("For up-to-date information, please visit:")
       .Append
         ("https://github.com/alire-project/alire/blob/master/UPGRADING.md"));

end Alr.Commands.Topics.Upgrading_Alr;
