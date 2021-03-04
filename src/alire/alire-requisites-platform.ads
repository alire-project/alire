with Alire.Conditional;
with Alire.Platforms;
with Alire.Properties.Platform;

with Alire.Requisites.Cases;
with Alire.TOML_Keys;

with TOML;

package Alire.Requisites.Platform with Preelaborate is

   package Ps   renames Platforms;
   package PrPl renames Properties.Platform;

   --  Packages used in new index, purely case-based.

   package Distro_Cases is new Cases
     (Enum      => Ps.Distributions,
      Property  => PrPl.Distributions.Property,
      Element   => PrPl.Distributions.Element,
      Name      => "Distribution",
      TOML_Name => TOML_Keys.Distribution);

   package OS_Cases is new Cases
     (Enum      => Ps.Operating_Systems,
      Property  => PrPl.Operating_Systems.Property,
      Element   => PrPl.Operating_Systems.Element,
      Name      => "OS",
      TOML_Name => TOML_Keys.OS);

   package Toolchain_Cases is new Cases
     (Enum      => Ps.Toolchains,
      Property  => PrPl.Toolchains.Property,
      Element   => PrPl.Toolchains.Element,
      Name      => "Toolchain",
      TOML_Name => TOML_Keys.Toolchain);

   package Word_Size_Cases is new Cases
     (Enum      => Ps.Word_Sizes,
      Property  => PrPl.Word_Sizes.Property,
      Element   => PrPl.Word_Sizes.Element,
      Name      => "Word_Size",
      TOML_Name => TOML_Keys.Word_Size);

end Alire.Requisites.Platform;
