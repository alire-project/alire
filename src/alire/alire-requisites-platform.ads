with Alire.Conditional;
with Alire.Platforms;
with Alire.Properties.Platform;

with Alire.Requisites.Cases;
with Alire.Requisites.Comparables;
with Alire.TOML_Keys;

with TOML;

package Alire.Requisites.Platform with Preelaborate is

   package Ps   renames Platforms;
   package PrPl renames Properties.Platform;

   use all type Tree;

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

   --  Packages used in Alire.Index, e.g., old more general expressions.
   --  TODO: remove during the old index Alire.Index dead code removal

   package Op_Systems is new Comparables
     (Ps.Operating_Systems, Ps."<", Ps.Operating_Systems'Image,
      PrPl.Operating_Systems.Property,
      PrPl.Operating_Systems.Element,
      "OS");

   package Op_System_Cases is new Conditional.For_Properties.Case_Statements
     (Ps.Operating_Systems, Op_Systems.Is_Equal_To);

   package Distributions is new Comparables
     (Ps.Distributions, Ps."<", Ps.Distributions'Image,
      PrPl.Distributions.Property,
      PrPl.Distributions.Element,
      "Distribution");

   package Distribution_Cases_Deps
   is new Conditional.For_Dependencies.Case_Statements
     (Ps.Distributions, Distributions.Is_Equal_To);

   package Distribution_Cases_Props
   is new Conditional.For_Properties.Case_Statements
     (Ps.Distributions, Distributions.Is_Equal_To);

   package Targets is new Comparables
     (Ps.Targets, Ps."<", Ps.Targets'Image,
      PrPl.Targets.Property,
      PrPl.Targets.Element,
      "Target");

   package Word_Sizes is new Comparables
     (Ps.Word_Sizes, Ps."<", Ps.Word_Sizes'Image,
      PrPl.Word_Sizes.Property,
      PrPl.Word_Sizes.Element,
      "Word_Size");

end Alire.Requisites.Platform;
