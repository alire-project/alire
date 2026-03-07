with Ada.Containers.Indefinite_Doubly_Linked_Lists; use Ada.Containers;
with Ada.Iterator_Interfaces;

with Alire.Interfaces;
with Alire.Properties;
with Alire.Utils.YAML;

private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Vectors;

with TOML; use all type TOML.Any_Value_Kind;

generic
   type Values (<>) is
     new Interfaces.Classifiable
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
   --  Single-line image for single-line tree image (used by Available).

   procedure Print (This    : Node;
                    Prefix  : String;
                    Verbose : Boolean;
                    Sorted  : Boolean)
   is abstract;
   --  Multi-line printing to stdout with tabulation (used by Props and
   --  Deps). Sorting affects only multi-value nodes (vectors, cases) and is
   --  interesting for dependencies (to show them alphabetically) but not for
   --  properties (some of them have order, like actions).

   function Leaf_Count (This : Node) return Positive is abstract;
   --  Return leaves under this node; for non-leaf nodes, obtain recursively.

   procedure Recursive_Traversal
     (This  : in out Node;
      Apply : access procedure (Value : in out Values)) is abstract;
   --  Enables full traversal with modification of children

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
   --  Recursive type that stores values, possibly with case expressions.
   --  Cases must be satisfied by some environment property or else their
   --  associated values will be dropped from the tree. This structure is thus
   --  used to store conditional/dynamic properties and dependencies.
   --  Iteration is only over direct children, when the tree is AND/OR vector.

   function Flatten (This : Node) return Tree'Class is abstract
   with Post'Class => Flatten'Result.Is_Empty or else
                      Flatten'Result.Is_Value or else
                      Flatten'Result.Is_Vector;
   --  Recursively merge all subtree elements in a single value or vector. It
   --  can result in an empty tree if a vector is empty, so it returns a tree.

   function Root (This : Tree) return Node'Class
     with Pre => not This.Is_Empty;

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
      --  This Append must honor "append" semantics (i.e., don't reorder);
      --  otherwise actions, that have a user-defined order, would break.
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

   procedure Print (This    : Tree;
                    Prefix  : String  := "";
                    Verbose : Boolean := False;
                    And_Or  : Boolean := True;
                    Sorted  : Boolean := False);
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

   procedure Append (L : in out Tree; R : Tree);
   --  Same as L := L and R;

   function "or" (L, R : Tree) return Tree;

   type Conjunctions is (Anded, Ored);

   function Is_Vector (This : Tree) return Boolean;

   function Conjunction (This : Tree) return Conjunctions
     with Pre => This.Root in Vector_Node;

   procedure Visit_All
     (This  : in out Tree;
      Apply : access procedure (Value : in out Values));
   --  Depth-first recursive traversal of all values, irrespective of node type

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

   -----------------
   --  ITERATORS  --
   -----------------

   --  This iterator works only on Value/Vector nodes and is not recursive.

   type Cursor is private;

   function Has_Element (This : Cursor) return Boolean;

   function Next (This : Cursor) return Cursor;

   package Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (Container : Tree) return Iterators.Forward_Iterator'Class
     with Pre => Container.Is_Iterable;
   --  Returns our own iterator.

   function Indexed_Element (Container : Tree; Pos : Cursor) return Tree;

   function To_Tree (N : Node'Class) return Tree;

   package Value_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Values);

   function As_List (This : Tree) return Value_Lists.List;
   --  Default Enumerate implementation. Remember that this does not resolve
   --  expressions; merely flattens all leaf nodes.

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
   function Flatten (This : Leaf_Node) return Tree'Class;

   overriding
   function Is_Conditional (N : Leaf_Node) return Boolean;

   overriding
   function Image (V : Leaf_Node) return String;

   overriding
   function Leaf_Count (This : Leaf_Node) return Positive;

   overriding
   procedure Print (This    : Leaf_Node;
                    Prefix  : String;
                    Verbose : Boolean;
                    Sorted  : Boolean := False);

   overriding
   procedure Recursive_Traversal
     (This  : in out Leaf_Node;
      Apply : access procedure (Value : in out Values));

   overriding
   procedure To_TOML (This : Leaf_Node; Parent : TOML.TOML_Value);

   overriding
   function To_YAML (V : Leaf_Node) return String;

   function Is_Value (This : Tree) return Boolean is
     (not This.Is_Empty and then This.Root in Leaf_Node);

   function Value (This : Tree) return Values is
     (Leaf_Node (This.Root).Value.Element);

   overriding
   function Contains_ORs (This : Leaf_Node) return Boolean is (False);

   overriding
   function Evaluate (This    : Leaf_Node;
                      Unused  : Properties.Vector)
                      return Tree'Class is (New_Leaf (This.Value.Element));

   overriding
   function Flatten (This : Leaf_Node) return Tree'Class is (To_Tree (This));

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
   function Flatten (This : Vector_Node) return Tree'Class;

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
   procedure Print (This    : Vector_Node;
                    Prefix  : String;
                    Verbose : Boolean;
                    Sorted  : Boolean);

   overriding
   procedure Recursive_Traversal
     (This  : in out Vector_Node;
      Apply : access procedure (Value : in out Values));

   overriding
   procedure To_TOML (This : Vector_Node; Parent : TOML.TOML_Value);

   overriding
   function To_YAML (V : Vector_Node) return String;

   function Is_Vector (This : Tree) return Boolean is
     (not This.Is_Empty and then This.Root in Vector_Node);

   --  Delayed implementation to avoid freezing:

   function Is_Iterable (This : Tree) return Boolean is
      (This.Is_Empty or else This.Is_Value or else This.Is_Vector);

   function Leaf_Count (This : Tree) return Natural is
     (if This.Is_Empty
      then 0
      else This.Root.Leaf_Count);

   function Root (This : Tree) return Node'Class is
     (This.Element);

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
   --  Keys containing one dot are split as nested tables. More than one dot
   --  is an error.

end Alire.Conditional_Trees;
