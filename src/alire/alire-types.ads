with Alire.Conditional;
with Alire.Dependencies;
--  with Alire.Dependencies.Vectors;
with Alire.Releases;

with Semantic_Versioning.Extended;

package Alire.Types is

   --  Recopilation of types for convenient use and documentation

   subtype Dependency is Dependencies.Dependency;
   --  A single dependency on a single crate+versions

   subtype Abstract_Dependencies is Conditional.Dependencies;
   --  Conditional dependencies as yet unmaterialized for a precise platform

   subtype Platform_Dependencies is Conditional.Platform_Dependencies;
   --  A plain tree without conditions (but might have OR nodes)

   subtype Forbidden_Dependencies is Conditional.Forbidden_Dependencies;
   --  A plain tree without conditions or alternatives

   function No_Dependencies return Conditional.Dependencies
     renames Conditional.For_Dependencies.Empty;

   function New_Dependency
     (Name     : Crate_Name;
      Versions : Semantic_Versioning.Extended.Version_Set)
      return Platform_Dependencies
     renames Conditional.New_Dependency;

   subtype Release is Releases.Release;
   --  A catalogued release

end Alire.Types;
