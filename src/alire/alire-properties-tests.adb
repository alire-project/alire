with Alire.Warnings;

package body Alire.Properties.Tests is

   -------------
   -- To_TOML --
   -------------

   overriding
   function To_TOML (S : Settings) return TOML.TOML_Value is
   begin
      --  Just an empty array
      return TOML.Create_Array;
   end To_TOML;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML
     (From : TOML_Adapters.Key_Queue) return Conditional.Properties
   is
      use TOML;
      Raw : TOML_Value;
   begin
      if From.Unwrap.Kind /= TOML_Table then
         From.Checked_Error
           ("test: table with assignments expected, but got: "
            & From.Unwrap.Kind'Img);
      end if;

      if From.Pop_Single_Table (Raw, TOML_Table) /= TOML_Keys.Test then
         raise Program_Error;
         --  Can't happen, unless the dispatch to us itself was erroneous
      end if;

      Warnings.Warn_Once
        ("Discarding future feature in manifest: built-in testsuite");

      return Conditional.No_Properties;
   end From_TOML;
end Alire.Properties.Tests;
