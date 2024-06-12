with Semantic_Versioning;

package Alire.Loading with Preelaborate is

   --  Auxiliary type to pass info around while loading things from disk
   --  (currently only indexes...)

   type Kinds is (None, Index);

   type Metadata (Kind : Kinds := None) is record
      case Kind is
         when Index =>
            Version : Semantic_Versioning.Version;
         when None =>
            null;
      end case;
   end record;

   function For_Index (Version : Semantic_Versioning.Version)
                       return Metadata
   is (Kind => Index, Version => Version);

   function No_Metadata return Metadata is (Kind => None);

end Alire.Loading;
