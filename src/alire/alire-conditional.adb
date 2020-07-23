with TOML;

package body Alire.Conditional is

   ---------------
   -- From_TOML --
   ---------------

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

end Alire.Conditional;
