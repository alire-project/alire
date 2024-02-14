with Semantic_Versioning;

package Alire.Index.Features is

   --  For proper lockstep updates, we keep track of features that we will
   --  enable in future index versions.

   subtype Version is Semantic_Versioning.Version;

   use type Version;

   Explicit_Binary_Origin : constant Version := +"1.3.0";
   --  Require that binary origins are explicitly marked as such instead of
   --  relying on dynamic expressions.

end Alire.Index.Features;
