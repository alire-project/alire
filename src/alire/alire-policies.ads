package Alire.Policies with Preelaborate is

   type For_Index_Merging is
     (Merge_Priorizing_Existing
      --  Merge two crates but any existing releases will be kept over ones
      --  with the same version when adding more release. This is the only
      --  behavior existing since multiple indexes were introduced.
      --
      --  For the case of externals, the shared properties of the first time a
      --  crate externals are added take precedence as well.

      --  We might envision other policies, like not allowing releases from two
      --  indexes at the same time, keeping only the first seen or overriding
      --  with the last seen.
     );

end Alire.Policies;
