package Alire.Externals.Unindexed with Preelaborate is

   --  A do-nothing external that is used to signal a known but unavailable
   --  crate.

   type External is new Externals.External with null record;

   overriding
   function Detect (This : External) return Containers.Release_Set is
     (Containers.Release_Sets.Empty_Set);

   overriding
   function Image (This : External) return String is
     ("Not yet available through the Alire project");

end Alire.Externals.Unindexed;
