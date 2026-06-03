package Alire.Test with Preelaborate is

   --  TODO: document in user changes

   Pragma_Name : constant String := "Alire_Test";
   --  The pragma name recognized by the alr test runner.

   --  Test configuration is achieved through pragmas in the context part:
   --
   --  `pragma Alire_Test (<key>, <value>);`
   --          |           |      |
   --          |           |      \- Value: the setting to apply
   --          |           \- Key: the option to exercise
   --          \- The pragma may appear many times
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
      --  When True, the file is a support unit (not a test main) and must not
      --  be turned into a test.
      --  Value is an optional Boolean.
      --  TODO: recognized but not yet implemented.
      --  TODO: we can also not try to turn into a test a .adb that has a
      --  corresponding .ads, since that's for sure a package. So this might be
      --  useful only for `separate` definitions.
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
