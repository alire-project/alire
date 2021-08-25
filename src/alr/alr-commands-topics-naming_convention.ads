with AAA.Strings;

with SubCommander;

with Alire.Crates;

package Alr.Commands.Topics.Naming_Convention is

   type Topic is new SubCommander.Help_Topic with null record;

   overriding
   function Name (This : Topic) return String
   is ("identifiers");

   overriding
   function Title (This : Topic) return String
   is ("Naming rules for crate and index names");

   overriding
   function Content (This : Topic) return AAA.Strings.Vector
   is (Alire.Crates.Naming_Convention);

end Alr.Commands.Topics.Naming_Convention;
