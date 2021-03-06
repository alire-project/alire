with Ada.Containers.Indefinite_Ordered_Maps;

with Alire.Errors;
with Alire.TOML_Adapters;

package body Alire.Expressions is

   package Key_Variable_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (String, Variable);

   package Variable_Value_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (String, Values'Class);

   Variables : Key_Variable_Maps.Map;

   Variable_Values : Variable_Value_Maps.Map;
   --  Stores values for all types that have been declared. Indexed by the
   --  Variable key.

   ----------
   -- From --
   ----------

   function From (Key : String) return Variable
   is (if not Variables.Contains (Key)
       then raise Checked_Error with
         Errors.Set ("Expression variable '" & Key & "' is unknown")
       else Variables (Key));

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : Variable; Value : String) return Boolean
   is (if not Variable_Values.Contains (Key (This))
       then raise Checked_Error with
         Errors.Set ("Expression variable '" & Key (This) & "' is unknown")
       else Variable_Values (Key (This)).Is_Valid (Value));

   -----------------------
   -- Register_Variable --
   -----------------------

   procedure Register (Var_Key    : String;
                       Var_Name   : String;
                       Var_Values : Values'Class)
   is
   begin
      Variables.Insert (Var_Key, Variable'(Key  => +Var_Key,
                                           Name => +Var_Name));
      Variable_Values.Insert (Var_Key, Var_Values);
   end Register;

   ---------------
   -- Satisfies --
   ---------------

   function Satisfies (Property : Properties.Property'Class;
                       Var_Key  : String;
                       Value    : String) return Boolean
   is (Property.Key = Var_Key and then
       Variables.Contains (Var_Key) and then
       From (Var_Key).Is_Valid (Value) and then
       TOML_Adapters.Tomify (Property.Image) = TOML_Adapters.Tomify (Value));

end Alire.Expressions;
