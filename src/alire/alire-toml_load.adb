with Alire.Errors;

with TOML;

package body Alire.TOML_Load is

   pragma Warnings (Off);
   --  Temporary until implementation is completed in later commits.

   ------------------------
   -- Load_Crate_Section --
   ------------------------

   function Load_Crate_Section (Section : Projects.Sections;
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
      --  TODO: Load dependencies (in upcoming commit)

      --  TODO: Process Forbidden

      --  TODO: Load Available (in upcoming commit)

      --  Process remaining keys, which must be properties
      --  TODO: Load Properties (in upcoming commit)

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
