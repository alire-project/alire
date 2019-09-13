package Alire.Origins.Tweaks is

   --  Separate package for non-preelaborable operations.

   function Fixed_Origin (TOML_Path : String;
                          This      : Origin) return Origin;
   --  TOML_Path is the path to the file that contains the crate description.
   --  If the origin is not local nothing is done. Otherwise, the relative
   --  path in This is made absolute rooted from TOML_Path. This operation is
   --  necessary because, during index loading, the origin that parses itself
   --  is unable to check the filesystem (the information is not available
   --  to it), so it is performed after TOML parsing by the TOML_Index load
   --  callers.

end Alire.Origins.Tweaks;
