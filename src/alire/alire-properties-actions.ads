with Alire.Conditional;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;

package Alire.Properties.Actions with Preelaborate is

   type Moments is
     (
      Post_Fetch, -- After being downloaded
      Pre_Build,  -- Before being compiled as the working release
      Post_Build, -- After being compiled as the working release
      Test,       -- On demand for testing of releases
      On_Demand   -- On demand from command-line
     );

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
