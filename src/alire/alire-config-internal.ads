package Alire.Config.Internal with Preelaborate is

   --  Configuration keys that are used internally by Alire, not intended to be
   --  set or used by users directly. They're all under the "alire" table.

   type Keys is (Shared_Dependencies);
   --  TODO: Migrate here some keys that are scattered elsewhere, like build
   --  profiles and external toolchain at least.

   function Key (K : Keys) return String;

end Alire.Config.Internal;