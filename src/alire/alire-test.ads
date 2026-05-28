package Alire.Test with Preelaborate is

   type Unknown_Parameter_Action is
     (Ignore,
      --  Silently ignore unknown Alire_Test pragma keys and hope for the best.
      Fail,
      --  Report the test as failed without spawning it.
      Skip);
      --  Report the test as skipped without spawning it.
   --  Valid values for the `tests.on_unknown_parameter` setting, which
   --  selects how the runner reacts to unrecognized Alire_Test pragma keys.

   type Pragmas is
     (Auxiliary_File,
      --  When True, the file is a support unit (not a test main) and must
      --  not be turned into a test. (TODO: recognized but not yet implemented.)
      Name,
      --  Override the displayed test name.
      Should_Fail,
      --  When True, the test is expected to fail and a failure counts as pass.
      Timeout);
      --  Per-test deadline in seconds (Integer or Real).
      --  (TODO: recognized but not yet implemented.)
   --  Documented keys accepted in `pragma Alire_Test (<key>, <value>);`.
   --  See scripts/schemas/test-pragmas.yaml for the canonical schema.

end Alire.Test;
