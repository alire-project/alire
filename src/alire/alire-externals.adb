with Alire.TOML_Expressions.Cases;
with Alire.TOML_Keys;

with TOML;

package body Alire.Externals is

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue) return External'Class is
      use type Requisites.Tree;

      TOML_Avail : TOML.TOML_Value;
      pragma Warnings (Off);
      Result     : External'Class := From_TOML (From); -- Recursive until impl
      pragma Warnings (On);
   begin
      --  Process Available
      if From.Pop (TOML_Keys.Available, TOML_Avail) then
         Result.Available := Result.Available and
           TOML_Expressions.Cases.Load_Requisites
             (TOML_Adapters.From (TOML_Avail,
                                  From.Message (TOML_Keys.Available)));
      end if;

      raise Unimplemented; -- No concrete externals defined yet
      return Result;
   end From_TOML;

end Alire.Externals;
