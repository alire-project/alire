with Alire.Conditional_Trees;
with Alire.Dependencies;
with Alire.Properties;
with Alire.Requisites;

with Semantic_Versioning;

package Alire.Conditional with Preelaborate is

   package For_Dependencies is new Conditional_Trees (Dependencies.Dependency,
                                                       Dependencies.Image);
   subtype Dependencies is For_Dependencies.Tree;

   subtype Platform_Dependencies is Conditional.Dependencies
     with Dynamic_Predicate => Platform_Dependencies.Is_Unconditional;
   --  A plain tree without conditions (but might have OR nodes)

   subtype Forbidden_Dependencies is Platform_Dependencies
     with Dynamic_Predicate => not Forbidden_Dependencies.Contains_ORs;
   --  A plain tree without conditions or alternatives

   function New_Dependency (Name     : Alire.Project;
                            Versions : Semantic_Versioning.Version_Set)
                            return Dependencies;

   package For_Properties
   is new Conditional_Trees (Properties.Property'Class,
                             Properties.Image_Classwide);
   subtype Properties is For_Properties.Tree;

   function New_Property (Property : Alire.Properties.Property'Class)
                          return Properties;

private

   function New_Dependency (Name     : Alire.Project;
                            Versions : Semantic_Versioning.Version_Set)
                            return Dependencies is
     (For_Dependencies.New_Value
        (Alire.Dependencies.New_Dependency (Name, Versions)));

   function New_Property (Property : Alire.Properties.Property'Class)
                          return Properties is
     (For_Properties.New_Value (Property));

end Alire.Conditional;
