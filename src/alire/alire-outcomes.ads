package Alire.Outcomes with Preelaborate is

   --  The generic children of this package allow to tie together an Outcome
   --  and a result value. This ensures that the result value cannot be used in
   --  case of Outcome_Failure, or bugs where the result is not set despite
   --  returning an Outcome_Success.

   --  Both Definite and Indefinite share the same operations.

end Alire.Outcomes;
