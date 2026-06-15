package body Alire.Origins.Mirrors is

   ---------------
   -- From_Toml --
   ---------------

   procedure From_TOML (From    : TOML_Adapters.Key_Queue;
                        This    : in out Mirror_Vector;
                        Primary : Origins.Origin)
   is
      pragma Unreferenced (This, Primary);
   begin
      if From.Contains (TOML_Keys.Mirror) then
         From.Unwrap.Unset (TOML_Keys.Mirror);
         --  Drop the key for now to pass loading tests
      end if;

      --  TODO: actually implement loading
   end From_Toml;

   --------------
   -- Whenever --
   --------------

   function Whenever (This : Mirror_Vector;
                      Env  : Properties.Vector)
                      return Mirror_Vector
   is
   begin
      return Result : Mirror_Vector do
         for Mirror of This loop
            Result.Append (Mirror.Whenever (Env));
         end loop;
      end return;
   end Whenever;

end Alire.Origins.Mirrors;