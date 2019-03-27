with Alire.Conditional;

with Semantic_Versioning;
with Semantic_Versioning.Expressions;

package Alire.Versions with Preelaborate is

   --  Helper package to prepare expressions on version for use in Alire.Index.*

   type Versioned is interface;

   function Project (V : Versioned) return Alire.Project is abstract;

   function Version (V : Versioned) return Semantic_Versioning.Version is abstract;

   function Version_Classwide (V : Versioned'Class) return Semantic_Versioning.Version is (V.Version);

   function This_Version (V : Versioned'Class) return Conditional.Dependencies;
   function Within_Major (V : Versioned'Class) return Conditional.Dependencies;
   function Within_Minor (V : Versioned'Class) return Conditional.Dependencies;

   type Comparable is interface;

   function New_Dependency (L : Comparable; VS : Semantic_Versioning.Version_Set)
                            return Conditional.Dependencies is abstract;

   function New_Dependency_Classwide (L : Comparable'Class; VS : Semantic_Versioning.Version_Set)
                                      return Conditional.Dependencies is (L.New_Dependency (VS));

   package Expressions is new Semantic_Versioning.Expressions
     (Comparable'Class,
      Conditional.Dependencies,
      New_Dependency_Classwide);

   package Expressions_With_Versioned is new Expressions.Against (Versioned'Class,
                                                                  Version_Classwide);

   --  Utils dealing with versions

   function From_Identifier (S : String) return Semantic_Versioning.Version;
   --  Convert an Ada Identifier into a version
   --  This is used by the package releases in Index to autodetect the version
   --    and avoid duplication
   --  EXAMPLES OF VALID IDENTIFIERS:
   --    V1_2_3_Prerelease_Build
   --    V_1_2_3

private

   use Semantic_Versioning;

   function This_Version (V : Versioned'Class) return Conditional.Dependencies is
     (Conditional.New_Dependency (V.Project, Exactly (V.Version)));

   function Within_Major (V : Versioned'Class) return Conditional.Dependencies is
     (Conditional.New_Dependency (V.Project, Within_Major (V.Version)));

   function Within_Minor (V : Versioned'Class) return Conditional.Dependencies is
     (Conditional.New_Dependency (V.Project, Within_Minor (V.Version)));

end Alire.Versions;
