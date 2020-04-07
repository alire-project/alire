with Ada.Containers; use Ada.Containers;
with Ada.Iterator_Interfaces;

with Alire.Interfaces;
with Alire.Properties;
with Alire.Requisites;
with Alire.Utils.YAML;

private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Vectors;

with TOML; use all type TOML.Any_Value_Kind;

generic
   type Values (<>) is
     new Interfaces.Classificable
     and Interfaces.Tomifiable
     and Interfaces.Yamlable
   with private;

   with function Image (V : Values) return String;
package Alire.Conditional_Trees with Preelaborate is

   ----------
   -- Node --
   ----------

   --  To allow unforeseeable case nodes, we use classwide nodes in the tree.
   --  A few of those nodes are already known here and can be used elsewhere,
   --  like leaf nodes (a single value) and vector nodes (anded/ored subtrees).
   --  Condition nodes check a single requisite tree as if/then/else.
   --  Case nodes, defined in the generic child Conditional_Trees.Cases, are a
   --  more compact alternative to if/then/elif/elif/elif/else trees.

   type Node is abstract new Interfaces.Yamlable with private;

   function Contains_ORs (This : Node) return Boolean is abstract;
   --  A tree/node containing OR nodes requires selection of one of these
   --  during dependency resolution.

   function Is_Conditional (This : Node) return Boolean is abstract;
   --  Recursively say if there are conditional nodes under this one.
   --  In general, we say a tree is conditional if it contains expressions that
   --  depend on environment properties. In the old index, those where the
   --  if/then/else expressions, and in the new index there are case exprs.
   --  Prior to dependency resolution, a tree must be evaluated using the
   --  available properties to remove conditional expressions.

   function Image (This : Node) return String is abstract;
   --  Single-line image for single-line tree image (used by Requisites).

   procedure Print (This : Node; Prefix : String; Verbose : Boolean)
   is abstract;
   --  Multi-line printing to stdout with tabulation (used by Props and Deps).

   function Leaf_Count (This : Node) return Positive is abstract;
   --  Return leaves under this node; for non-leaf nodes, obtain recursively.

   function Flatten (This : Node) return Node'Class is abstract;
   --  with Post'Class => Flatten'Result in Leaf_Node or else
   --                     Flatten'Result in Vector_Node;
   --  Above Post kept for reference but gnat bugs out during instantiation.
   --  Recursively merge all subtree elements in a single value or vector.
   --  Since it cannot result in an empty tree, it returns a proper node.

   procedure To_TOML (This : Node; Parent : TOML.TOML_Value) is abstract with
     Pre'Class => Parent.Kind = TOML.TOML_Table;

   ----------
   -- Tree --
   ----------

   type Tree is
     new Interfaces.Tomifiable
     and Interfaces.Yamlable
   with private with
     Default_Iterator => Iterate,
     Iterator_Element => Tree,
     Constant_Indexing => Indexed_Element;
   --  Recursive type that stores values, possibly with associated requisites.
   --  Requisites must be satisfied by some environment property or else their
   --  associated values will be dropped from the tree. This structure is thus
   --  used to store conditional/dynamic properties and dependencies.
   --  Iteration is only over direct children, when the tree is AND/OR vector.

   function Root (This : Tree) return Node'Class;

   function Is_Iterable (This : Tree) return Boolean;
   --  Iterators visit only immediate values (leaves or vectors). Thus, this
   --  function says if a tree root node is a value or a vector, and can be
   --  iterated over.

   function Leaf_Count (This : Tree) return Natural;
   --  Recursive descent to obtain the leaf count of the tree.

   generic
      type Collection is private;
      with procedure Append (C     : in out Collection;
                             V     : Values;
                             Count : Count_Type := 1);

   function Materialize (This    : Tree;
                         Against : Properties.Vector)
                         return Collection with
     Pre => not This.Contains_ORs;
   --  Materialize against the given properties, and return as list. NOTE:
   --  this presumes there are no OR conditions along the tree. In Alire
   --  context, this is always true for properties and potentially never
   --  for dependencies (so, for the latter, must be used after dependency
   --  resolution).

   generic
      type Collection is private;
      with procedure Append (C     : in out Collection;
                             V     : Values;
                             Count : Count_Type := 1);
   function Enumerate (This : Tree) return Collection;
   --  Return all value nodes, regardless of conditions/conjunctions.
   --  This is used for textual search and has no semantic meaning.

   function Evaluate (This : Tree; Against : Properties.Vector) return Tree
     with Post => Evaluate'Result.Is_Unconditional;
   --  Materialize against the given properties, returning values as an
   --  unconditional tree. NOTE: the result is unconditional but can still
   --  contain a mix of AND/OR subtrees.

   function Is_Empty (This : Tree) return Boolean;

   function Empty return Tree;

   function Image_One_Line (This : Tree) return String;
   --  See corresponding Node.Image_One_Line.

   function Is_Unconditional (This : Tree) return Boolean;
   --  Recursively looks for nodes that are not leaves or conjunctions.
   --  TODO: refactor as Is_Conditional to match the Node one.

   function Contains_ORs (This : Tree) return Boolean;

   --  Delayed node primitives that require the Tree type follow --

   function Evaluate (This    : Node;
                      Against : Properties.Vector)
                      return Tree'Class is abstract with
     Post'Class => Evaluate'Result.Is_Unconditional;
   --  Check properties in conditional nodes to return the applicable elements.
   --  Returns a Tree because it could result in an empty tree.

   procedure Print (This   : Tree;
                    Prefix : String := "";
                    And_Or : Boolean := True);
   --  Use And_Or = false when only And can appear, in which case there is no
   --  need to distinguish and the output is slightly more compact.

   overriding
   function To_TOML (This : Tree) return TOML.TOML_Value
     with Post => To_TOML'Result.Kind = TOML.TOML_Table;
   --  Every tree element can provide a key under which to be filed
   --  This is used for key = value leaves and [table] names in non-leaves.

   overriding
   function To_YAML (This : Tree) return String;

   --  The basic known node classes follow. Extra classes for case expressions
   --  are defined in the Cases child package. This enables having any number
   --  of environment variables properly typed in the tree, including future
   --  unforeseen ones.

   --------------
   --  LEAVES  --
   --------------

   type Leaf_Node is new Node with private;
   --  A leaf node stores a single static value.

   function New_Leaf (V : Values) return Tree;

   function New_Value (V : Values) return Tree renames New_Leaf;

   function Is_Value (This : Tree) return Boolean;

   function Value (This : Tree) return Values
     with Pre => This.Root in Leaf_Node;

   ---------------
   --  VECTORS  --
   ---------------

   type Vector_Node is new Node with private;
   --  A vector node is a conjunction or disjunction of the list of stored
   --  child nodes.

   function "and" (L, R : Tree) return Tree;
   --  Concatenation

   function "and" (L : Tree; R : Values) return Tree is
      ("and" (L, New_Value (R)));

   function "or" (L, R : Tree) return Tree;

   type Conjunctions is (Anded, Ored);

   function Is_Vector (This : Tree) return Boolean;

   function Conjunction (This : Tree) return Conjunctions
     with Pre => This.Root in Vector_Node;

   --  Following iterators/accessors are used during dependency resolution, and
   --  for that reason they will fail for conditional trees.

   procedure Iterate_Children (This    : Tree;
                               Visitor : access procedure (CV : Tree)) with
     Pre => This.Root in Leaf_Node or else
            This.Root in Vector_Node;
   --  There is "of" notation below, but that one sometimes causes compilation
   --  bugs when using this package as generic formal.

   function First_Child (This : Tree) return Tree;
   --  This, when This is a leaf, or the first child, when a vector. Error
   --  otherwise.

   function All_But_First_Children (This : Tree) return Tree;
   --  Empty, when This is a leaf, or all children but first, when vector.
   --  Error otherwise.

   --------------------
   --  CONDITIONALS  --
   --------------------

   --  Conditional nodes are no longer used with the new index syntax. They may
   --  be kept around in case at some point the syntax is expanded.

   type Conditional_Node is new Node with private;
   --  A conditional node stores a if/then/else structure, based on whether its
   --  requisites are fulfilled or not.

   function New_Conditional (If_X   : Requisites.Tree;
                             Then_X : Tree;
                             Else_X : Tree) return Tree;

   function Condition (This : Tree) return Requisites.Tree
     with Pre => This.Root in Conditional_Node;

   function True_Value (This : Tree) return Tree
     with Pre => This.Root in Conditional_Node;

   function False_Value (This : Tree) return Tree
     with Pre => This.Root in Conditional_Node;

   --  The following generic transforms an array of some enumerated type that
   --  holds further conditional subtrees into an if/elif/elif/elif/else tree.
   --  This was used by the old index and is superseded by the new compact Case
   --  nodes, which result in a flat structure closer to the TOML syntax.

   generic
      type Enum is (<>);
      with function Requisite_Equal (V : Enum) return Requisites.Tree;
      --  Function which creates an equality requisite on V
   package Case_Statements is

      type Arrays is array (Enum) of Tree;

      function Case_Is (Arr : Arrays) return Tree;

   end Case_Statements;

   -----------------
   --  ITERATORS  --
   -----------------

   --  This iterator works only on Value/Vector nodes and is not recursive.

   type Cursor is private;

   function Has_Element (This : Cursor) return Boolean;

   function Next (This : Cursor) return Cursor;

   package Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (Container : Tree) return Iterators.Forward_Iterator'Class
     with Pre => Container.Is_Empty or else
                 Container.Root in Leaf_Node or else
                 Container.Root in Vector_Node;
   --  Returns our own iterator.

   function Indexed_Element (Container : Tree; Pos : Cursor) return Tree;

   function To_Tree (N : Node'Class) return Tree;

   ---------------
   --  QUERIES  --
   ---------------

   function Contains (Container : Tree; Value : Values) return Boolean;
   --  Say if the tree contains the value at any depth

private

   type Node is abstract new Interfaces.Yamlable with null record;

   function Image_Classwide (This : Node'Class) return String;

   package Holders
   is new Ada.Containers.Indefinite_Holders (Node'Class);

   package Vectors
   is new Ada.Containers.Indefinite_Vectors (Positive, Node'Class);

   type Cursor is new Vectors.Cursor;

   ----------
   -- Tree --
   ----------

   type Tree is
     new Holders.Holder
     and Interfaces.Tomifiable
     and Interfaces.Yamlable
   with null record;
   --  Instead of dealing with pointers and finalization, we use this
   --  class-wide container. At some point it might be more efficient to use
   --  the Ada.Containers.Indefinite_Multiway_Trees.

   function To_Tree (N : Node'Class) return Tree is (To_Holder (N));

   package Definite_Values is new Ada.Containers.Indefinite_Holders (Values);

   ---------------
   -- Leaf Node --
   ---------------

   type Leaf_Node is new Node with record
      Value : Definite_Values.Holder;
   end record;

   overriding
   function Contains_ORs (This : Leaf_Node) return Boolean;

   overriding
   function Evaluate (This    : Leaf_Node;
                      Unused  : Properties.Vector) return Tree'Class;

   overriding
   function Flatten (This : Leaf_Node) return Node'Class;

   overriding
   function Is_Conditional (N : Leaf_Node) return Boolean;

   overriding
   function Image (V : Leaf_Node) return String;

   overriding
   function Leaf_Count (This : Leaf_Node) return Positive;

   overriding
   procedure Print (This : Leaf_Node; Prefix : String; Verbose : Boolean);

   overriding
   procedure To_TOML (This : Leaf_Node; Parent : TOML.TOML_Value);

   overriding
   function To_YAML (V : Leaf_Node) return String;

   function Is_Value (This : Tree) return Boolean is
     (This.Root in Leaf_Node);

   function Value (This : Tree) return Values is
     (Leaf_Node (This.Root).Value.Element);

   overriding
   function Contains_ORs (This : Leaf_Node) return Boolean is (False);

   overriding
   function Evaluate (This    : Leaf_Node;
                      Unused  : Properties.Vector)
                      return Tree'Class is (New_Leaf (This.Value.Element));

   overriding
   function Flatten (This : Leaf_Node) return Node'Class is (This);

   overriding
   function Is_Conditional (N : Leaf_Node) return Boolean is (False);

   overriding
   function Leaf_Count (This : Leaf_Node) return Positive is (1);

   -----------------
   -- Vector Node --
   -----------------

   type Vector_Node is new Node with record
      Conjunction : Conjunctions;
      Values      : Vectors.Vector;
   end record;

   overriding
   function Contains_ORs (This : Vector_Node) return Boolean is
     (This.Conjunction = Ored or else
        (for some Child of This.Values => Child.Contains_ORs));

   overriding
   function Is_Conditional (N : Vector_Node) return Boolean is
     (for some Child of N.Values => Child.Is_Conditional);

   overriding
   function Leaf_Count (This : Vector_Node) return Positive;

   overriding
   function Evaluate (This    : Vector_Node;
                      Against : Properties.Vector)
                      return Tree'Class;

   overriding
   function Flatten (This : Vector_Node) return Node'Class;

   function Conjunction (This : Vector_Node) return Conjunctions;

   package Non_Primitive is

      --  This package is used for internal operations that we do not need to
      --  be primitive, and to prevent freezing while making these available
      --  to other primitives below.

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

      function To_YAML is new Utils.YAML.To_YAML
        (Node'Class,
         Vectors,
         Vectors.Vector);

   end Non_Primitive;

   overriding function Image (V : Vector_Node) return String;

   overriding
   procedure Print (This : Vector_Node; Prefix : String; Verbose : Boolean);

   overriding
   procedure To_TOML (This : Vector_Node; Parent : TOML.TOML_Value);

   overriding
   function TO_YAML (V : Vector_Node) return String;

   function Is_Vector (This : Tree) return Boolean is
     (This.Root in Vector_Node);

   ----------------------
   -- Conditional Node --
   ----------------------

   type Conditional_Node is new Node with record
      Condition  : Requisites.Tree;
      Then_Value : Tree;
      Else_Value : Tree;
   end record;

   overriding
   function Contains_ORs (This : Conditional_Node) return Boolean is
      (This.Then_Value.Contains_ORs or else This.Else_Value.Contains_ORs);

   overriding
   function Is_Conditional (N : Conditional_Node) return Boolean is (True);

   overriding
   function Image (V : Conditional_Node) return String;

   overriding
   function To_YAML (V : Conditional_Node) return String;

   overriding
   function Flatten (This : Conditional_Node) return Node'Class is
     (Flatten (Tree'Class (This.Then_Value and This.Else_Value).Root));

   overriding
   function Leaf_Count (This : Conditional_Node) return Positive is
     (This.Then_Value.Leaf_Count + This.Else_Value.Leaf_Count);

   overriding
   function Evaluate (This    : Conditional_Node;
                      Against : Properties.Vector)
                      return Tree'Class is
     (if This.Condition.Check (Against)
      then This.Then_Value.Evaluate (Against)
      else This.Else_Value.Evaluate (Against));

   overriding
   procedure Print (This    : Conditional_Node;
                    Prefix  : String;
                    Verbose : Boolean);

   overriding
   procedure To_TOML (This : Conditional_Node; Parent : TOML.TOML_Value);

   --  Delayed implementation to avoid freezing:

   function Is_Iterable (This : Tree) return Boolean is
      (This.Is_Value or else This.Is_Vector);

   function Leaf_Count (This : Tree) return Natural is
     (if This.Is_Empty
      then 0
      else This.Root.Leaf_Count);

   function Root (This : Tree) return Node'Class is
     (This.Constant_Reference);

   procedure Tree_TOML_Add (Table : TOML.TOML_Value;
                            Key   : String;
                            Val   : TOML.TOML_Value);
   --  For the benefit of node tomification:
   --  Add one property to the parent table.
   --  Atomic values are automatically converted into arrays, if
   --    more than one for the same key appears (e.g., executables)
   --  Table values with same key are merged in a single table (e.g.,
   --  dependencies)
   --  Array values with same key are consolidated in a single array
   --    (e.g., actions, which are created as an array of tables).

end Alire.Conditional_Trees;
