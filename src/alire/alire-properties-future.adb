with Alire.Warnings;

package body Alire.Properties.Future is

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties
   is
      use TOML;
      use type Conditional.Properties;
   begin

      --  Pop first the key.value table artificially constructed by the parser

      if From.Unwrap.Kind /= TOML_Table then
         From.Checked_Error
           ("future: table with assignments expected, but got: "
            & From.Unwrap.Kind'Img);
      end if;

      --  Now, the value can be an actual table or an scalar, depending on the
      --  actual manifest schema. For future properties we do not want to be
      --  overly picky as things might change, so accept whatever.

      declare
         Raw : TOML_Value;
         Key : constant String := From.Pop (Raw);
      begin
         Warnings.Warn_Once
           ("Discarding future property in manifest: " & Key
            & "(" & Raw.Kind'Image & ")");

         return Conditional.No_Properties
           and Property'(Name_Len => Key'Length,
                         Name     => Key,
                         Object   => Raw);
      end;
   end From_TOML;

end Alire.Properties.Future;
