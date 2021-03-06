package body Alire.Expressions.Maps is

   ------------
   -- Insert --
   ------------

   procedure Insert (M : in out Map; V : String; E : Elements)
   is
   begin
      M.Entries.Insert (V, E);
   end Insert;

   ----------------
   -- Set_Others --
   ----------------

   procedure Set_Others (M : in out Map; E : Elements) is
   begin
      M.Insert (TOML_Keys.Case_Others, E);
   end Set_Others;

end Alire.Expressions.Maps;
