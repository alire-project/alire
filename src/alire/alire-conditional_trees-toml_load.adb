package body Alire.Conditional_Trees.TOML_Load is

   function Load (From    : TOML_Adapters.Key_Queue;
                  Loader  : not null Static_Loader;
                  Resolve : Boolean;
                  Strict  : Boolean)
                  return Tree
   is (raise Unimplemented);

end Alire.Conditional_Trees.TOML_Load;
