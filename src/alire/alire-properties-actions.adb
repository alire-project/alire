with Alire.Properties.Actions.Runners;

package body Alire.Properties.Actions is

   -------------
   -- Execute --
   -------------

   procedure Execute (This : Action;
                      Implementer : access procedure (This : Action'Class))
   is
   begin
      Implementer (This);
   end Execute;

   --  We redispatch TOML serialization to the Run class, which currently
   --  implements all of it, being the only existing Action class.

   -------------
   -- To_TOML --
   -------------

   function To_TOML_CW (This : Action'Class) return TOML.TOML_Value
   is (This.To_TOML);

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties
   is (Runners.From_TOML (From));

end Alire.Properties.Actions;
