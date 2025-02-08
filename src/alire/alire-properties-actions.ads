with Alire.Conditional;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;

package Alire.Properties.Actions with Preelaborate is

   Action_Failed : exception;
   --  Raised by the action runner when action execution fails (exit code /= 0)

   type Moments is
     (
      Post_Fetch, -- After being downloaded/on dependency updates
      Pre_Build,  -- Before being compiled
      Post_Build, -- After being compiled
      Test,       -- On demand for testing of releases
      On_Demand   -- On demand from command-line
     );

   --  The "lifecycle" of releases in Alire is described here. For all pre/post
   --  actions, dependencies are visited in a safe order, and the root is
   --  visited last. E.g., for the following dependency graph:

   --  R +-> A --> B --|
   --    |-> D --------+-> C

   --  C will be visited first. Then B and D will be visited, potentially
   --  simultaneously. Then, A will be visited, and then R will be visited.

   --  In the following descriptions, "for all releases" includes releases
   --  user-pinned as a link to a folder/remote.

   --  * Post_Fetch is triggered for all releases whenever there is a change in
   --  dependencies, which includes just after retrieval via `alr get` or `alr
   --  with`. In addition, running `alr update` will also trigger Post_Fetch,
   --  even when there are no updates to apply.

   --  * Pre_Build is triggered for all releases whenever a build is going to
   --  happen, which currently is on `alr build`, `alr run`, `alr get --build`.

   --  * Post_Build is triggered for all releases after a build completes.

   --  * Test is triggered only for the root crate after the crate build (and
   --  after all Post_Build complete), only when `alr test` is run.

   type Action (<>) is abstract new Properties.Property with private;
   --  Action was abstract in case we ever need other kinds of actions than
   --  running custom commands (see the Run action). The need hasn't arisen
   --  yet.

   overriding
   function Key (This : Action) return String is (TOML_Keys.Action);

   function Moment (This : Action) return Moments;

   procedure Execute (This : Action;
                      Implementer : access procedure (This : Action'Class));
   --  This indirection is meant to keep this package preelaborable, as the
   --  rest of the properties hierarchy.

   --  Note that the TOML crate spec does not reflect the type/moment
   --  difference; moments are used as the class of the action.

   function To_TOML_CW (This : Action'Class) return TOML.TOML_Value;

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties;

private

   type Action (Moment : Moments)
   is abstract new Properties.Property with null record;

   function Moment (This : Action) return Moments is (This.Moment);

end Alire.Properties.Actions;
