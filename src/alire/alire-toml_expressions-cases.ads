with Alire.Conditional;
with Alire.TOML_Adapters;

package Alire.TOML_Expressions.Cases with Preelaborate is

   function Load_Dependencies (From : TOML_Adapters.Key_Queue)
                               return Conditional.Dependencies;
   --  From is the RHS of a "depends-on = { ... }" table.

   subtype Static_Loader is Conditional.Property_Loader;

   function Load_Property (Key    : String;
                           From   : TOML_Adapters.Key_Queue;
                           Loader : Static_Loader)
                           return Conditional.Properties;
   --  Expects a "key = from" table.
   --  To be used during resolution of a dynamic property expression. Only a
   --  particular property static value is accepted when in an expr
   --  (because the syntax is key.expr.values, and not expr.key.values, so
   --  key and values must agree). We explicitly pass a Loader for the property
   --  that is being resolved.

   function Load_Availability (From : TOML_Adapters.Key_Queue)
                               return Conditional.Availability;
   --  From is an "available = ..." table.

end Alire.TOML_Expressions.Cases;
