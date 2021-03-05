package body Alire.Conditional is

   -------------------------
   -- Available_From_TOML --
   -------------------------

   function Available_From_TOML (From : TOML_Adapters.Key_Queue)
                                 return For_Available.Tree
   is
   begin
      if From.Unwrap.Is_Null or else From.Unwrap.Keys'Length = 0 then
         return For_Available.Empty;
      else
         return For_Available.New_Value
           (Available'
              (Is_Available =>
               From.Checked_Pop
                 (TOML_Keys.Available,
                  TOML.TOML_Boolean).As_Boolean));
      end if;
   end Available_From_TOML;

   --------------------
   -- Deps_From_TOML --
   --------------------

   function Deps_From_TOML (From : TOML_Adapters.Key_Queue) return Dependencies
   is
      use type Dependencies;
   begin
      return Result : Dependencies do
         for Pair of From.Pop.Iterate_On_Table loop
            Result := Result and
              Alire.Dependencies.From_TOML (+Pair.Key,
                                            Pair.Value);
         end loop;
      end return;
   end Deps_From_TOML;

   ------------------
   -- Is_Available --
   ------------------

   function Is_Available (This : Availability;
                          Env  : Alire.Properties.Vector)
                          return Boolean
   is
      Tree : constant Availability := This. Evaluate (Env);

      -------------------
      -- Eval_Children --
      -------------------

      function Eval_Children (T : For_Available.Tree) return Boolean is
         use For_Available;
      begin
         if T.Root in Leaf_Node'Class then
            return T.Value.Is_Available;
         else
            return Result : Boolean := True do
               for Child of T loop
                  case T.Conjunction is
                     when Anded => Result := Result and Eval_Children (Child);
                     when Ored  => Result := Result or Eval_Children (Child);
                  end case;
               end loop;
            end return;
         end if;
      end Eval_Children;

   begin

      --  Trivial case out of the way

      if This.Is_Empty or else Tree.Is_Empty then
         return True;
      end if;

      --  After evaluation, the tree is made of values/vectors that we can
      --  recursively evaluate.

      return Eval_Children (For_Available.Tree (Tree));

   end Is_Available;

end Alire.Conditional;
