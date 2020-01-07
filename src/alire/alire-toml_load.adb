with Alire.Errors;
with Alire.Properties.From_TOML;
with Alire.TOML_Expressions.Cases;

with TOML;

package body Alire.TOML_Load is

   ------------------------
   -- Load_Crate_Section --
   ------------------------

   function Load_Crate_Section (Section : Crates.Sections;
                                From    : TOML_Adapters.Key_Queue;
                                Props   : in out Conditional.Properties;
                                Deps    : in out Conditional.Dependencies;
                                Avail   : in out Requisites.Tree)
                                return Outcome
   is
      use type Conditional.Dependencies;
      use type Conditional.Properties;
      use type Requisites.Tree;

      TOML_Avail : TOML.TOML_Value;
      TOML_Deps  : TOML.TOML_Value;
   begin
      --  Process Dependencies
      if From.Pop ("depends-on", TOML_Deps) then
         Deps := Deps and
           TOML_Expressions.Cases.Load_Dependencies
             (TOML_Adapters.From (TOML_Deps, From.Message ("depends-on")));
      end if;

      --  TODO: Process Forbidden

      --  Process Available
      if From.Pop ("available", TOML_Avail) then
         Avail := Avail and
           TOML_Expressions.Cases.Load_Requisites
             (TOML_Adapters.From (TOML_Avail, From.Message ("available")));
      end if;

      --  Process remaining keys, which must be properties
      Props := Props and
        Properties.From_TOML.Section_Loaders (Section) (From);

      return Outcome_Success;
   exception
      when E : Checked_Error =>
         return Errors.Get (E);
      when E : others =>
         Log_Exception (E);
         return Outcome_Failure ("Unexpected condition: "
                                 & From.Message (Errors.Get (E)));
   end Load_Crate_Section;

end Alire.TOML_Load;
