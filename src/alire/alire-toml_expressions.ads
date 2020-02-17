with Alire.TOML_Adapters;
with Alire.Requisites;

with TOML;

package Alire.TOML_Expressions with Preelaborate is

   --  This package is the core of loading and holding unresolved dynamic case
   --  expressions. This is achieved through three generic packages below.

   --  The first one, Enum_Cases, defines a loader for TOML case expressions
   --  that only loads one such expression, for a concrete enumeration type.

   --  The second one, Enum_Trees, takes a tree that can hold case expressions
   --  and defines a Load function that is able to detect case expressions in
   --  the midst of loading a tree. As argument, this function must receive
   --  a function that redirects to the loader for a particular case-typed
   --  expression.

   --  Finally, Tree_Builders defines a function that can load any case(xx)
   --  expression, and that for non-case values (or expressions of a different
   --  case type) redirects to the loader in Enum_Trees, which in turn will
   --  either load a static value, or redirect to the proper case type loader.

   --  Since there is a circularity here (Enum_Trees requires all case loaders,
   --  for all known types, which are provided by Tree_Builders, which in turn
   --  cannot be instantiated without first instantiating Tree_Builders), this
   --  circularity is broken by Enum_Trees accepting a function that returns
   --  the pointers to Tree_Builders.Load_Case instances. This function is
   --  defined externally, and relies on information gathered after all
   --  instances have been created.

   --  TODO: we can drop the strong-typing for these expressions, which is no
   --  longer necessary with the new index since these types are not seen by
   --  users of the index or used after a conditional tree is resolved for
   --  use with the dependency solver. This will greatly reduce the convoluted
   --  generics that we have currently. We can do this transparently at a later
   --  time since it only concerns internals. I (álex) also would like to
   --  be completely sure that indeed we do not need the strong typing for
   --  anything useful.

   Case_Prefix : constant String := "case(";
   Dots        : constant String := "...";

   type Case_Loader_Keys is (Distribution,
                             OS,
                             Word_Size);
   --  The variables that can be used in index cases. Must match the toml text.

   function Contains_Expression (Value : TOML.TOML_Value) return Boolean;
   --  Check if Value contains some case(xx) key.

   generic
      type Enum is (<>);
   package Enum_Cases is

      --  This package is the simplest one, just loading a single case(xx).

      type TOML_Array is array (Enum) of TOML.TOML_Value;
      --  Immediate TOML value for each case.

      function Load_Cases (From : TOML_Adapters.Key_Queue) return TOML_Array;
      --  Intermediate loader that does not resolve leaves.
      --  May raise Checked_Error if a case entry is missing.

   end Enum_Cases;

   generic
      type Tree is private;
      with function "and" (L, R : Tree) return Tree;
      with function Default return Tree with Warnings => Off;
      --  We allow omitting alternatives in cases, even without '...'. This
      --  default applies then; which is True for boolean expressions (like
      --  in Ada (if Cond then Bool [else True]), and also like when the
      --  "available" field is not given. For Props/Deps, it is an empty list.
      --  Warnings (Off) applied because it is unreferenced in this package,
      --  but used down the road by other instances.
   package Enum_Trees is

      --  A tree of values that will be have leaves containing cases with
      --  values. Either Boolean_Trees, for requisites, or Conditional_Trees,
      --  for Dependencies and Properties.

      type Static_Tree_Loader is not null access
        function (From : TOML_Adapters.Key_Queue) return Tree;
      --  Static loaders receive the pair data (key = whatever} that has to be
      --  used to build values. For some, key may be redundant, but properties
      --  need it to discriminate among them.

      type Recursive_Case_Loader is access
        function (Parent        : String;
                  From          : TOML_Adapters.Key_Queue;
                  Static_Loader : Static_Tree_Loader;
                  Unused_Marker : Integer := 0) return Tree;
      --  The recursive loader prototype matches the Tree_Builders.Load_Cases
      --  function, with an extra dummy parameter to avoid confusion with the
      --  following Load function. The difference between the following Load
      --  and Tree_Builders.Load_Cases is as follows: Load_Cases does the
      --  actual loading of a case, recursively invoking Load for the actual
      --  case values. Load, in turn dispatches to the proper type Load_Cases
      --  when a case is encountered. These functions could both be in
      --  this package but the we could not use proper named types for the
      --  function prototypes, making everything (even more) confusing.
      --  Recursive_Case_Loaders are all known at runtime, and so they can
      --  be returned by the following Loaders formal to the Load function.

      generic
         type Case_Keys is (<>);
         with function Loaders (Key : Case_Keys) return Recursive_Case_Loader;
      function Load (Parent        : String;
                     From          : TOML_Adapters.Key_Queue;
                     Static_Loader : Static_Tree_Loader) return Tree;
      --  Entry point into loading expression trees. Identifies case(xx)
      --  expressions, which are recursivley loaded using the Loaders, or using
      --  Static_Loader for final values. Parent is the "key" being loaded.
      --  From points to the RHS value or case expr May raise Checked_Error.

   end Enum_Trees;

   generic
      with package Trees is new Enum_Trees (<>);
      with package Cases is new Enum_Cases (<>);
      --  Instances for the enumeration we are loading.

      type Enum_Array is array (Cases.Enum) of Trees.Tree;
      with function New_Leaf (Cases : Enum_Array) return Trees.Tree;
      --  Creation of a leaf holding a case expression.

      with function Load (Parent        : String;
                          From          : TOML_Adapters.Key_Queue;
                          Static_Loader : Trees.Static_Tree_Loader)
                          return Trees.Tree;
      --  The static loader for values of the enumeration type (the one
      --  instantiated from the above Enum_Trees instance, that knows how
      --  to redispatch in case of a different type case expression.
   package Tree_Builders is

      function Load_Cases (Parent        : String;
                           From          : TOML_Adapters.Key_Queue;
                           Static_Loader : Trees.Static_Tree_Loader;
                           Unused_Marker : Integer := 0)
                           return Trees.Tree;
      --  Parent is the actual key of the expr being loaded ("case(xx)" text).
      --  From points to a case(xx) table of alternatives for Enum_Cases. Each
      --  case value will be recursively loaded, be it static or an expr, using
      --  the Load generic formal.

   end Tree_Builders;

end Alire.TOML_Expressions;
