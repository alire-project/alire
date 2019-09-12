with Alire.TOML_Load;

package body Alire.Projects is

   ---------------
   -- From_TOML --
   ---------------

   overriding
   function From_TOML (This : in out General;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome
   is
      Result : constant Outcome :=
                 TOML_Load.Load_Crate_Section
                   (General_Section,
                    From,
                    This.Properties,
                    This.Dependencies,
                    This.Available);

   begin
      if not Result.Success then
         return Result;
      end if;

      --  Check for remaining keys, which must be erroneous.
      --  TODO: Should we instead emit a warning (or make it an option) to
      --  allow for unforeseen future keys in the index?
      return From.Report_Extra_Keys;
   end From_TOML;

end Alire.Projects;
