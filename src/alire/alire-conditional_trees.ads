with Ada.Containers; use Ada.Containers;
with Ada.Iterator_Interfaces;

with Alire.Interfaces;
with Alire.Properties;
with Alire.Requisites;
with Alire.Utils;

private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Vectors;

with TOML;

generic
   type Values (<>) is new Interfaces.Tomifiable with private;
   with function Image (V : Values) return String;
package Alire.Conditional_Trees with Preelaborate is

   type Kinds is (Condition, Value, Vector);

   type Tree is new Interfaces.Tomifiable with private with
     Default_Iterator => Iterate,
     Iterator_Element => Tree,
     Constant_Indexing => Indexed_Element;
   --  Recursive type that stores conditions (requisites) and values/further conditions if they are met or not
   --  Iteration is only over direct children, when the tree is AND/OR list

   function Leaf_Count (This : Tree) return Natural;

   generic
      type Collection is private;
      with procedure Append (C : in out Collection; V : Values; Count : Count_Type := 1);
   function Materialize (This : Tree; Against : Properties.Vector) return Collection;
   --  Materialize against the given properties, and return as list
   --  NOTE: this presumes there are no OR conditions along the tree
   --  In Alire context, this is always true for properties and
   --    potentially never for dependencies

   generic
      type Collection is private;
      with procedure Append (C : in out Collection; V : Values; Count : Count_Type := 1);
   function Enumerate (This : Tree) return Collection;
   --  Return all value nodes, regardless of dependencies/conjunctions
   --  This is used for textual search and has no semantic trascendence

   function Evaluate (This : Tree; Against : Properties.Vector) return Tree;
   --  Materialize against the given properties, returning values as an unconditional tree
   --  NOTE: the result is unconditional but can still contain a mix of AND/OR subtrees

   function Kind (This : Tree) return Kinds;

   function Is_Empty (This : Tree) return Boolean;

   function Empty return Tree;

   function Image_One_Line (This : Tree) return String;

   function Is_Unconditional (This : Tree) return Boolean;
   -- Recursively!

   function Contains_ORs (This : Tree) return Boolean;

   ---------------
   --  SINGLES  --
   ---------------

   function New_Value (V : Values) return Tree; -- when we don't really need a condition

   function Value (This : Tree) return Values
     with Pre => This.Kind = Value;

   ---------------
   --  VECTORS  --
   ---------------

   function "and" (L, R : Tree) return Tree;
   --  Concatenation

   function "or" (L, R : Tree) return Tree;

   type Conjunctions is (Anded, Ored);

   function Conjunction (This : Tree) return Conjunctions
     with Pre => This.Kind = Vector;

   procedure Iterate_Children (This    : Tree;
                               Visitor : access procedure (CV : Tree));
   --  There is "of" notation too, but that bugs out when using this package as generic formal

   type Children_Array is array (Positive range <>) of Tree;

   function First_Child (This : Tree) return Tree;

   function All_But_First_Children (This : Tree) return Tree;

   --------------------
   --  CONDITIONALS  --
   --------------------

   function New_Conditional (If_X   : Requisites.Tree;
                             Then_X : Tree;
                             Else_X : Tree) return Tree;

   function Condition (This : Tree) return Requisites.Tree
     with Pre => This.Kind = Condition;

   function True_Value (This : Tree) return Tree
     with Pre => This.Kind = Condition;

   function False_Value (This : Tree) return Tree
     with Pre => This.Kind = Condition;

   generic
      type Enum is (<>);
      with function Requisite_Equal (V : Enum) return Requisites.Tree;
      --  Function which creates an equality requisite on V
   package Case_Statements is

      type Arrays is array (Enum) of Tree;

      function Case_Is (Arr : Arrays) return Tree;

   end Case_Statements;

   -----------
   -- Print --
   -----------

   procedure Print (This   : Tree;
                    Prefix : String := "";
                    And_Or : Boolean := True);
   --  And_Or is false if only And can appear, thus no necessity to distinguish

   -------------
   -- To_TOML --
   -------------

   function To_TOML (This : Tree) return TOML.TOML_Value is
      (raise Program_Error with "TODO: implement");

   -----------------
   --  ITERATORS  --
   -----------------

   type Cursor is private;

   function Has_Element (This : Cursor) return Boolean;

   function Next (This : Cursor) return Cursor;

   package Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (Container : Tree)
      return Iterators.Forward_Iterator'Class;
   -- Returns our own iterator, which in general will be defined in the
   -- private part or the body.

   function Indexed_Element (Container : Tree; Pos : Cursor)
      return Tree;

private

   type Inner_Node is interface;

   function Image (Node : Inner_Node) return String is abstract;

   function Image_Classwide (Node : Inner_Node'Class) return String;

   function Kind (This : Inner_Node'Class) return Kinds;

   package Holders is new Ada.Containers.Indefinite_Holders (Inner_Node'Class);
   package Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Inner_Node'Class);

   type Cursor is new Vectors.Cursor;

   type Tree is new Holders.Holder and Interfaces.Tomifiable with null record;
   --  Instead of dealing with pointers and finalization, we use this class-wide container

   package Definite_Values is new Ada.Containers.Indefinite_Holders (Values);

   type Value_Inner is new Inner_Node with record
      Value : Definite_Values.Holder;
   end record;

   overriding function Image (V : Value_Inner) return String;

--     overriding function To_Code (This : Tree) return Utils.String_Vector;

   type Vector_Inner is new Inner_Node with record
      Conjunction : Conjunctions;
      Values      : Vectors.Vector;
   end record;

   function Conjunction (This : Vector_Inner) return Conjunctions;

   package Non_Primitive is
      function One_Liner_And is new Utils.Image_One_Line
        (Vectors,
         Vectors.Vector,
         Image_Classwide,
         " and ",
         "(empty condition)");

      function One_Liner_Or is new Utils.Image_One_Line
        (Vectors,
         Vectors.Vector,
         Image_Classwide,
         " or ",
         "(empty condition)");
   end Non_Primitive;

   overriding function Image (V : Vector_Inner) return String;

   type Conditional_Inner is new Inner_Node with record
      Condition  : Requisites.Tree;
      Then_Value : Tree;
      Else_Value : Tree;
   end record;

   overriding function Image (V : Conditional_Inner) return String;

   function As_Value (This : Tree) return Values
   with Pre => This.Kind = Value;

   function As_Conditional (This : Tree) return Conditional_Inner'Class
   with Pre => This.Kind = Condition;

   function As_Vector (This : Tree) return Vectors.Vector
   with Pre => This.Kind = Vector;

end Alire.Conditional_Trees;
