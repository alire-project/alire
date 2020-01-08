with Alire.Platforms;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;

package Alire.Properties.Platform with Preelaborate is

   --  The following packages declare types used by requisites so they have to
   --  be public.

   pragma Warnings (Off); -- unreferenced galore follows

   function Distro_Key (D : Platforms.Distributions) return String
   is (TOML_Keys.Distribution);

   function Tomify is new TOML_Adapters.Tomify_Enum (Platforms.Distributions);
   package Distributions is new Values (Platforms.Distributions,
                                        Platforms.Distributions'Image,
                                        Platforms.Distributions'Image,
                                        Distro_Key,
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

   function Word_Size_Key (WS : Platforms.Word_Sizes) return String
   is (TOML_Keys.Word_Size);
   function Tomify is new TOML_Adapters.Tomify_Enum (Platforms.Word_Sizes);
   package Word_Sizes is new Values (Platforms.Word_Sizes,
                                     Platforms.Word_Sizes'Image,
                                     Platforms.Word_Sizes'Image,
                                     Word_Size_Key,
                                     Tomify);

   pragma Warnings (On);

   function Distribution_Is (D : Platforms.Distributions) return Vector
   renames Distributions.New_Vector;

   function System_Is (OS : Platforms.Operating_Systems) return Vector
   renames Operating_Systems.New_Vector;

   function Target_Is (T : Platforms.Targets) return Vector
   renames Targets.New_Vector;

   function Word_Size_Is (V : Platforms.Word_Sizes) return Vector
   renames Word_Sizes.New_Vector;

end Alire.Properties.Platform;
