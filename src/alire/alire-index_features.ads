with Semantic_Versioning;

package Alire.Index_Features is

   --  For easier lockstep updates, we keep track of features that we will
   --  enable in future index versions.

   subtype Min_Version is Semantic_Versioning.Version;

   use type Min_Version;

   Explicit_Binary_Origin : constant Min_Version := +"1.3.0";
   --  Require that binary origins are explicitly marked as such instead of
   --  relying on dynamic expressions.

end Alire.Index_Features;
