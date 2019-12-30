with Alire;

with Semantic_Versioning.Extended;

package Alr.Parsers with Preelaborate is

   type Allowed_Milestones (Len : Positive) is record
      Crate    : Alire.Crate_Name (1 .. Len);
      Versions : Semantic_Versioning.Extended.Version_Set;
   end record;

   function Crate_Versions (Spec : String) return Allowed_Milestones;
   --  Either valid set or Constraint_Error
   --  If no version was specified, Any version is returned
   --  Syntax: name[extended version set expression]

end Alr.Parsers;
