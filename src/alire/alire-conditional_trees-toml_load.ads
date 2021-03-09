with Alire.TOML_Adapters;

generic
package Alire.Conditional_Trees.TOML_Load is

   --  NOTE: this package must be instantiated at library level

   type Static_Loader is access
     function (From : TOML_Adapters.Key_Queue) return Tree;
   --  A function that receives a table "key = ...", with key being the name of
   --  the value in the index (e.g., available, depends-on, executables, ...).
   --  The loaded values are returned already as an (unconditional) tree.

   function Load (From    : TOML_Adapters.Key_Queue;
                  Loader  : not null Static_Loader;
                  Resolve : Boolean;
                  Strict  : Boolean)
                  return Tree;
   --  Expects a "key = val" or "key.expr = val" table. Takes care of resolving
   --  any dynamic expressions. Currently, only 'case()' is understood. When
   --  resolve, dynamic expressions are resolved; otherwise Checked_Error will
   --  be raised. When Resolve and Strict, unknown values in cases are not
   --  allowed. If not Resolve, Strict is ignored.

end Alire.Conditional_Trees.TOML_Load;
