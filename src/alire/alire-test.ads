package Alire.Test with Preelaborate is

   Pragma_Name : constant String := "Alire_Test";
   --  The pragma name recognized by the alr test runner.

   --  A test is a parameterless main procedure under the test crate's src/
   --  that declares itself with an Alire_Test pragma in its context part.
   --  Sources that cannot be a runnable main (packages, functions, generics,
   --  subunits, ...) are ignored without needing any pragma.
   --
   --  `pragma Alire_Test;`           -- declare as a test, with defaults
   --  `pragma Alire_Test (<key> => <value);` -- alt syntax
   --  `pragma Alire_Test (<key>, <value>);`
   --          |           |      |
   --          |           |      \_ Value: the setting to apply
   --          |           \_ Key: the option to exercise
   --          \_ The pragma may appear many times
   --
   --  See /schemas/test-pragmas.yaml for the canonical schema.

   --  Valid values for the `tests.on_unknown_parameter` setting, which
   --  selects how the runner reacts to unrecognized Alire_Test pragma keys.
   type Unknown_Parameter_Action is
     (Ignore,
      --  Silently ignore unknown Alire_Test pragma keys and hope for the best.
      Fail,
      --  Report the test as failed without spawning it.
      Skip);
      --  Report the test as skipped without spawning it.

   --  Keys accepted in `pragma Alire_Test (<key>, <value>);`.
   type Pragmas is
     (Auxiliary_File,
      --  When True (or value omitted), the source is a support unit, not a
      --  test main, and is excluded from testing. Must be the only key in its
      --  Alire_Test configuration. Applies to files that could be a test only.
      --  Value is an optional Boolean.
      Name,
      --  Override the displayed test name.
      --  Value is a mandatory String.
      Should_Fail,
      --  When True, the test is expected to fail and a failure counts as pass.
      --  Value is an optional Boolean.
      Timeout
      --  Per-test deadline in seconds.
      --  Value is a Duration.
      --  (TODO: recognized but not yet implemented.)
      );

end Alire.Test;
