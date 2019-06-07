package Alire.Requisites.Booleans with Preelaborate is

   type Requisite_True is new Requisite with null record;

   type Requisite_False is new Requisite with null record;

   overriding
   function Is_Applicable (R      : Requisite_True;
                           Unused : Property'Class)
                           return Boolean
   is (True);

   overriding
   function Satisfies (R      : Requisite_True;
                       Unused : Property'Class)
                       return Boolean
   is (True);

   overriding
   function Image (R : Requisite_True) return String
   is ("True");

   overriding
   function Is_Applicable (R      : Requisite_False;
                           Unused : Property'Class)
                           return Boolean
   is (True);

   overriding
   function Satisfies (R      : Requisite_False;
                       Unused : Property'Class)
                       return Boolean
   is (False);

   overriding
   function Image (R : Requisite_False) return String
   is ("False");
   pragma Warnings (On);

   function Always_True  return Tree
   is (Trees.Leaf (Requisite_True'(null record)));

   function Always_False return Tree
   is (Trees.Leaf (Requisite_False'(null record)));

end Alire.Requisites.Booleans;
