with Alire.TOML_Keys;
with Alire.Utils.Switches; use Alire.Utils.Switches;

package body Alire.Properties.Build_Switches is

   --------------
   -- Modifier --
   --------------

   function Modifier (This : Variable)
                      return Alire.Utils.Switches.Modifiers.Profile_Modifier
   is
   begin
      return Alire.Utils.Switches.Modifiers.From_TOML (This.T);
   end Modifier;

   -----------
   -- Image --
   -----------

   overriding
   function Image (This : Variable) return String is
      ("Build Switches: ");

   ---------
   -- Key --
   ---------

   overriding
   function Key (This : Variable) return String is
      pragma Unreferenced (This);
   begin
      return TOML_Keys.Build_Profile;
   end Key;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties
   is
      use type Conditional.Properties;
      use TOML;
      Env : TOML_Value;

      Var : Variable;
   begin
      if From.Unwrap.Kind /= TOML_Table then
         From.Checked_Error
           ("Build_switches: table with assignments expected, but got: "
            & From.Unwrap.Kind'Img);
      end if;

      if From.Pop_Single_Table (Env, TOML_Table) /= TOML_Keys.Build_Switches
      then
         raise Program_Error;
         --  Can't happen, unless the dispatch to us itself was erroneous
      end if;

      return Props : Conditional.Properties do
         Var.T := Env.Clone;
         Props := Props and Var;
      end return;

   end From_TOML;

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (This : Variable) return TOML.TOML_Value is
   begin
      return This.T.Clone;
   end To_TOML;

   -------------
   -- To_YAML --
   -------------

   overriding
   function To_YAML (This : Variable) return String is
     ("Build switches: 'TODO'");

end Alire.Properties.Build_Switches;
