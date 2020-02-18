package Alire.Externals.Unindexed is

   --  A do-nothing external that is used to signal a known but unavailable
   --  crate.

   type External is new Externals.External with null record;

   overriding
   function Detect (This        : External;
                    Unused_Name : Crate_Name) return Containers.Release_Set is
     (Containers.Release_Sets.Empty_Set);

   overriding
   function Image (This : External) return String is ("Externally provided");

   overriding
   function Detail (This          : External;
                    Unused_Distro : Platforms.Distributions)
                    return Utils.String_Vector;
   --  Return all custom hints or else a default message

   overriding
   function Kind (This : External) return String is ("Hint");

end Alire.Externals.Unindexed;
