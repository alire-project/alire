with AAA.Strings;

with CLIC.Subcommander;

with Alire.Crates;

package Alr.Commands.Topics.Naming_Convention is

   type Topic is new CLIC.Subcommander.Help_Topic with null record;

   overriding
   function Name (This : Topic) return CLIC.Subcommander.Identifier
   is ("identifiers");

   overriding
   function Title (This : Topic) return String
   is ("Naming rules for crate and index names");

   overriding
   function Content (This : Topic) return AAA.Strings.Vector
   is (Alire.Crates.Naming_Convention);

end Alr.Commands.Topics.Naming_Convention;
