
with TOML; use TOML;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Alire.Conditional; use Alire.Conditional;

package body Alire.Properties.Templates is

   -------------------------
   -- Templates_From_TOML --
   -------------------------

   function Templates_From_TOML (From : TOML_Adapters.Key_Queue)
                                 return Conditional.Properties
   is
      use Conditional.For_Properties;

      ----------------
      -- Create_One --
      ----------------

      function Create_One (Name : String;
                           Raw  : TOML.TOML_Value)
                           return Conditional.Properties
      is
         Template_Input : constant Input_Definition :=
           (Def => Alire.Utils.Config_Type_Def.Type_Def_From_TOML
              (Name, Raw, TOML_Keys.Template_Inputs,
               Default_Required => True));
      begin
         return New_Value (Template_Input);
      end Create_One;

      Raw : constant TOML_Value := From.Pop;

   begin
      if Raw.Kind /= TOML_Table then
         Raise_Checked_Error (TOML_Keys.Template_Inputs & "must be a table");
      end if;

      return Props : Conditional.Properties do
         for Item of Iterate_On_Table (Raw) loop
            Trace.Error (To_String (Item.Key));

            Props := Props and Create_One (To_String (Item.Key),
                                           Item.Value);
            Raw.Unset (Item.Key);
         end loop;
      end return;
   end Templates_From_TOML;

end Alire.Properties.Templates;
