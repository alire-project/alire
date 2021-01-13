with Alire.Conditional;
with Alire.Licensing;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;
with Alire.Utils.YAML;

with TOML;

private with Ada.Containers.Indefinite_Holders;
private with SPDX;

package Alire.Properties.Licenses is

   --  Licenses can be either a value from the enumeration of known licenses,
   --  or a free custom text.

   type License is new Property with private;

   function New_License (From  : String) return License;
   --  Verify that From is a known license name, and create it. In other cases
   --  Checked_Error is raised.

   overriding
   function Key (Dummy_L : License) return String
   is (TOML_Keys.License);

   overriding
   function Image (L : License) return String;

   overriding
   function To_TOML (L : License) return TOML.TOML_Value;

   function From_TOML (From : TOML_Adapters.Key_Queue)
                       return Conditional.Properties;

   overriding
   function To_YAML (L : License) return String;

private

   Max_SPDX_Expression_Length : constant := 150;
   --  SPDX the license expression can be arbitrary long, so we cap the
   --  accepted size.

   function "=" (A, B : SPDX.Expression) return Boolean
   is (SPDX.Img (A) = SPDX.Img (B));

   package SPDX_Holder is new Ada.Containers.Indefinite_Holders
     (SPDX.Expression);

   use all type Licensing.Licenses;

   type License is new Property with record
      Holder : SPDX_Holder.Holder;
   end record;

   overriding
   function Image (L : License) return String is
     ("License: " & SPDX.Img (L.Holder.Element));

   overriding
   function To_TOML (L : License) return TOML.TOML_Value
   is (TOML.Create_String (+SPDX.Img (L.Holder.Element)));

   overriding
   function To_YAML (L : License) return String is
     (Alire.Utils.YAML.YAML_Stringify
        (Utils.Replace (L.Image, "License: ", "")));
   --  Remove the prefix "License: " which is not machine-intended.

end Alire.Properties.Licenses;
