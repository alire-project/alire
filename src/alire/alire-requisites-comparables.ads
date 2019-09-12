with Alire.Utils;

with TOML;

generic
   --  Encapsulated basic type
   type Value is private;
   with function "<" (L, R : Value) return Boolean;
   with function Image (V : Value) return String is <>;

   --  Encapsulating property
   type Property is new Properties.Property with private;
   with function Element (P : Property) return Value;

   Name : String;
   --  used for image "Name (operation) Mixed_Case (Image (Value))"
package Alire.Requisites.Comparables with Preelaborate is

   package Value_Requisites is new For_Property (Property);

   type Comparable (<>) is new Value_Requisites.Requisite with private;

   overriding
   function Is_Satisfied (R : Comparable; P : Property) return Boolean;
   overriding
   function Image        (R : Comparable) return String;

   not overriding
   function New_Comparable return Comparable;
   --  This is the root function that can be renamed to a sensible name to
   --  appear in expressions.

   generic
   function Factory return Comparable;
   --  Alternatively this makes for a simpler instantiation since no profile is
   --  needed.

   function "=" (L : Comparable; R : Value) return Tree;
   function "=" (L : Value; R : Comparable) return Tree;

   function "/=" (L : Comparable; R : Value) return Tree;
   function "/=" (L : Value; R : Comparable) return Tree;

   function "<" (L : Comparable; R : Value) return Tree;
   function "<" (L : Value; R : Comparable) return Tree;

   function "<=" (L : Comparable; R : Value) return Tree;
   function "<=" (L : Value; R : Comparable) return Tree;

   function ">" (L : Comparable; R : Value) return Tree;
   function ">" (L : Value; R : Comparable) return Tree;

   function ">=" (L : Comparable; R : Value) return Tree;
   function ">=" (L : Value; R : Comparable) return Tree;

   function Is_Equal_To (V : Value) return Tree;
   --  Non-operator function useful elsewhere for case statements

   overriding
   function To_TOML (This : Comparable) return TOML.TOML_Value is
     (raise Unimplemented);
   --  Should not currently appear in the toml index, unless syntax changes.

private

   type Kinds is (Base, Equality, Ordering);

   type Comparable (Kind : Kinds) is new Value_Requisites.Requisite with record
      Value : Comparables.Value;
   end record;

   not overriding
   function New_Comparable return Comparable is (Kind => Base, Value => <>);

   overriding
   function Is_Satisfied (R : Comparable; P : Property) return Boolean is
     (case R.Kind is
         when Base     => raise Constraint_Error
                          with "Is_Satisfied: Requisite without operation",
         when Equality => R.Value = Element (P),
         when Ordering => Element (P) < R.Value
     );

   overriding function Image (R : Comparable) return String is
     (case R.Kind is
         when Base     => raise Constraint_Error
                          with "Image: Requisite without operation",
         when Equality => Name & " = " & Utils.To_Mixed_Case (Image (R.Value)),
         when Ordering => Name & " < " & Utils.To_Mixed_Case (Image (R.Value))
     );

   function Factory return Comparable is (New_Comparable);

   use all type Tree;

   function "/=" (L : Comparable; R : Value) return Tree is (not (L = R));
   function "/=" (L : Value; R : Comparable) return Tree is (not (L = R));

   function "<=" (L : Comparable; R : Value) return Tree is (L < R or L = R);
   function "<=" (L : Value; R : Comparable) return Tree is (L < R or L = R);

   function ">" (L : Comparable; R : Value) return Tree is (not (L <= R));
   function ">" (L : Value; R : Comparable) return Tree is (not (L <= R));

   function ">=" (L : Comparable; R : Value) return Tree is (not (L < R));
   function ">=" (L : Value; R : Comparable) return Tree is (not (L < R));

   function "=" (L : Comparable; R : Value) return Tree is
     (Trees.Leaf (Comparable'(Kind => Equality, Value => R)));

   function "=" (L : Value; R : Comparable) return Tree is (R = L);

   function "<" (L : Comparable; R : Value) return Tree is
      (Trees.Leaf (Comparable'(Kind => Ordering, Value => R)));

   function "<" (L : Value; R : Comparable) return Tree is (R >= L);

   function Is_Equal_To (V : Value) return Tree is
      (Trees.Leaf (Comparable'(Kind => Equality, Value => V)));

end Alire.Requisites.Comparables;
