with Alire.Platforms;
with Alire.Properties.Cases;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;

package Alire.Properties.Platform with Preelaborate is

   --  The following packages declare types used by requisites so they have to
   --  be public.

   pragma Warnings (Off, "is not referenced"); -- unreferenced galore follows

   function Distro_Key (D : Platforms.Distributions) return String
   is (TOML_Keys.Distribution);

   function Tomify is new TOML_Adapters.Tomify_Enum (Platforms.Distributions);
   package Distributions is new Values (Platforms.Distributions,
                                        Platforms.Distributions'Image,
                                        Platforms.Distributions'Image,
                                        Distro_Key,
                                        Tomify);

   function Host_Arch_Key (A : Platforms.Architectures) return String
   is (TOML_Keys.Host_Arch);

   function Tomify is new TOML_Adapters.Tomify_Enum (Platforms.Architectures);
   package Host_Archs is new Values (Platforms.Architectures,
                                     Platforms.Architectures'Image,
                                     Platforms.Architectures'Image,
                                     Host_Arch_Key,
                                     Tomify);

   function OS_Key (OS : Platforms.Operating_Systems) return String
   is (TOML_Keys.OS);

   function Tomify is new TOML_Adapters.Tomify_Enum
     (Platforms.Operating_Systems);
   package Operating_Systems is new Values (Platforms.Operating_Systems,
                                            Platforms.Operating_Systems'Image,
                                            Platforms.Operating_Systems'Image,
                                            OS_Key,
                                            Tomify);

   function Target_Key (T : Platforms.Targets) return String
   is (TOML_Keys.Target);
   function Tomify is new TOML_Adapters.Tomify_Enum (Platforms.Targets);
   package Targets is new Values (Platforms.Targets,
                                  Platforms.Targets'Image,
                                  Platforms.Targets'Image,
                                  Target_Key,
                                  Tomify);

   function Toolchain_Key (T : Platforms.Toolchains) return String
   is (TOML_Keys.Toolchain);
   function Tomify is new TOML_Adapters.Tomify_Enum (Platforms.Toolchains);
   package Toolchains is new Values (Platforms.Toolchains,
                                     Platforms.Toolchains'Image,
                                     Platforms.Toolchains'Image,
                                     Toolchain_Key,
                                     Tomify);

   function Word_Size_Key (WS : Platforms.Word_Sizes) return String
   is (TOML_Keys.Word_Size);
   function Tomify is new TOML_Adapters.Tomify_Enum (Platforms.Word_Sizes);
   package Word_Sizes is new Values (Platforms.Word_Sizes,
                                     Platforms.Word_Sizes'Image,
                                     Platforms.Word_Sizes'Image,
                                     Word_Size_Key,
                                     Tomify);

   pragma Warnings (On);

   --  Case instantiations for case expressions

   package Ps   renames Platforms;
   package PrPl renames Properties.Platform;

   package Distro_Cases is new Cases
     (Enum      => Ps.Distributions,
      Property  => PrPl.Distributions.Property,
      Element   => PrPl.Distributions.Element,
      Name      => "Distribution",
      TOML_Name => TOML_Keys.Distribution);

   package Host_Arch_Cases is new Cases
     (Enum      => Ps.Architectures,
      Property  => PrPl.Host_Archs.Property,
      Element   => PrPl.Host_Archs.Element,
      Name      => "Architecture",
      TOML_Name => TOML_Keys.Host_Arch);

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

   function Host_Arch_Is (A : Platforms.Architectures) return Vector
   renames Host_Archs.New_Vector;

   function Distribution_Is (D : Platforms.Distributions) return Vector
   renames Distributions.New_Vector;

   function System_Is (OS : Platforms.Operating_Systems) return Vector
   renames Operating_Systems.New_Vector;

   function Target_Is (T : Platforms.Targets) return Vector
   renames Targets.New_Vector;

   function Toolchain_Is (T : Platforms.Toolchains) return Vector
   renames Toolchains.New_Vector;

   function Word_Size_Is (V : Platforms.Word_Sizes) return Vector
   renames Word_Sizes.New_Vector;

end Alire.Properties.Platform;
