with Alire;

with Semantic_Versioning;

package Alr.Parsers with Preelaborate is

   type Allowed_Milestones (Len : Positive) is record
      Project  : Alire.Project (1 .. Len);
      Versions : Semantic_Versioning.Version_Set;
   end record;

   function Project_Versions (Spec : String) return Allowed_Milestones;
   --  Either valid set or Constraint_Error
   --  If no version was specified, Any version is returned
   --  Syntax: name[(=|^|~)version]

end Alr.Parsers;
