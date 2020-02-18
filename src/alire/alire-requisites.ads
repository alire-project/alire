with Alire.Boolean_Trees;
with Alire.Interfaces;
with Alire.Properties;

package Alire.Requisites with Preelaborate is

   use Properties;

   type Requisite is abstract new
     Interfaces.Tomifiable with null record;
   --  A Requisite verifies against some internally stored data that a property
   --  is satisfied. Here we provide the basic storage of values but the actual
   --  checking function must be overridden for particular checks.

   function Is_Applicable (R : Requisite;
                           P : Property'Class)
                           return Boolean
                           is abstract;
   --  Initially there is no compatibility. See helper package below

   --  The following package is the building block to be used to define new
   --  compatibility checks. Here we tie a class of properties and requisites
   --  (e.g., versions and version sets) that make sense. A release has a
   --  list of properties, and a tree of requisites to be applied to potential
   --  dependencies.

   function Satisfies (R : Requisite;
                       P : Property'Class)
                       return Boolean
                       is abstract;
   --  This function is used later in the generic implementation to
   --  automatically downcast, so requisite implementations do not need to
   --  deal with this MI-mess

   function Satisfies (R : Requisite;
                       Unused_P : Property'Class;
                       Unused_V : Properties.Vector)
                       return Boolean
                       is (False);
   --  However, for the new case requisites, which in turn may contain other
   --  trees, we need to pass all properties to each of these requisite nodes
   --  so they can pass them down to their owned trees.
   --  These special requisites must override this alternative version.

   function Image (R : Requisite) return String is abstract;
   --  A necessary pain to be able to report

   --  Trees of requisites to be matched against a list of properties in a
   --  release

   function Satisfies (R : Requisite'Class;
                       P : Properties.Vector)
                       return Boolean;
   --  True if any of the properties in the vector satisfies the requisite

   function Image_Class (R : Requisite'Class) return String is (R.Image);

   package Trees is new Boolean_Trees (Properties.Vector,
                                       Requisite'Class,
                                       Satisfies,
                                       Image_Class);

   subtype Tree is Trees.Tree;
   use type Tree;

   function Default_To (This : Tree; Default : Tree) return Tree with
     Post => (if This.Is_Empty
              then Default_To'Result = Default
              else Default_To'Result = This);

   function No_Requisites return Trees.Tree is (Trees.Empty_Tree);
   --  Function instead of constant to keep Preelaborate

   ------------------
   -- For_Property --
   ------------------
   --  Using these we get free matching of properties to requisites
   --  It is in essence a work around MI

   generic
      type Matching_Property (<>) is new Property with private;
   package For_Property is

      type Requisite is abstract new Requisites.Requisite with null record;

      not overriding
      function Is_Satisfied (R : Requisite;
                             P : Matching_Property)
                             return Boolean
                             is abstract;
      --  This is the important function to override by Requisite
      --  implementations

      not overriding
      function Children_Are_Satisfied (R : Requisite;
                                       P : Matching_Property;
                                       Unused_V : Properties.Vector)
                                       return Boolean is
        (Requisite'Class (R).Is_Satisfied (P));
      --  The exhaustive form used only by 'case(xx)'
      --  It defaults to Is_Satisfied

      --  The remainder methods are utilities that do not require modifications
      --  by the client.

      overriding
      function Is_Applicable (R : Requisite;
                              P : Property'Class)
                              return Boolean
      is (P in Matching_Property);
      --  Convenience for the evaluator to determine which properties might
      --  satisfy a requisite

      overriding
      function Satisfies (R : Requisite; P : Property'Class) return Boolean is
        (if R.Is_Applicable (P)
         then Requisite'Class (R).Is_Satisfied (Matching_Property (P))
         else False);

      overriding
      function Satisfies (R : Requisite;
                          P : Property'Class;
                          V : Properties.Vector)
                          return Boolean
      is (if R.Is_Applicable (P)
          then Requisite'Class (R)
               .Children_Are_Satisfied (Matching_Property (P), V)
          else False);

   end For_Property;

end Alire.Requisites;
