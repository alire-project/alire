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
     (Name,
      --  Override the displayed test name.
      Timeout,
      --  Per-test deadline in seconds (Integer or Real).
      Should_Fail);
      --  When True, the test is expected to fail and a failure counts as pass.
   --  Documented keys accepted in `pragma Alire_Test (<key>, <value>);`.
   --  See scripts/schemas/test-pragmas.yaml for the canonical schema.

end Alire.Test;
