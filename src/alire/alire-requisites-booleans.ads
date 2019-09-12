with Alire.TOML_Adapters;

with TOML;

package Alire.Requisites.Booleans with Preelaborate is

   --  Special unconditional requisites to represent a plain Boolean.

   function Always_True return Tree;

   function Always_False return Tree;

   function New_Requisite (Bool : Boolean) return Tree;

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Tree;

private

   type Requisite is new Requisites.Requisite with record
      Bool : Boolean;
   end record;

   overriding
   function Image (R : Requisite) return String
   is (if R.Bool then "True" else "False");

   overriding
   function Is_Applicable (R      : Requisite;
                           Unused : Property'Class)
                           return Boolean
   is (True);

   overriding
   function Satisfies (R      : Requisite;
                       Unused : Property'Class)
                       return Boolean
   is (R.Bool);

   overriding
   function To_TOML (This : Requisite) return TOML.TOML_Value is
     (raise Unimplemented);

   function Always_True return Tree is
      (Trees.Leaf (Requisite'(Bool => True)));

   function Always_False return Tree is
     (Trees.Leaf (Requisite'(Bool => False)));

   function New_Requisite (Bool : Boolean) return Tree is
     (case Bool is
         when True  => Always_True,
         when False => Always_False);

end Alire.Requisites.Booleans;
