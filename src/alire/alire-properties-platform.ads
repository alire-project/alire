with Alire.Platforms;
with Alire.TOML_Adapters;
with Alire.TOML_Keys;

package Alire.Properties.Platform with Preelaborate is

   --  The following packages declare types used by requisites so they have to
   --  be public.

   pragma Warnings (Off); -- unreferenced galore follows

   function Compiler_Key (C : Platforms.Compilers) return String
   is (TOML_Keys.Compiler);

   function Tomify is new TOML_Adapters.Tomify (Platforms.Compilers);
   package Compilers is new Values (Platforms.Compilers,
                                    Platforms.Compilers'Image,
                                    Platforms.Compilers'Image,
                                    Compiler_Key,
                                    Tomify);

   function Distro_Key (D : Platforms.Distributions) return String
   is (TOML_Keys.Distribution);

   function Tomify is new TOML_Adapters.Tomify (Platforms.Distributions);
   package Distributions is new Values (Platforms.Distributions,
                                        Platforms.Distributions'Image,
                                        Platforms.Distributions'Image,
                                        Distro_Key,
                                        Tomify);

   function OS_Key (OS : Platforms.Operating_Systems) return String
   is (TOML_Keys.OS);

   function Tomify is new TOML_Adapters.Tomify (Platforms.Operating_Systems);
   package Operating_Systems is new Values (Platforms.Operating_Systems,
                                            Platforms.Operating_Systems'Image,
                                            Platforms.Operating_Systems'Image,
                                            OS_Key,
                                            Tomify);

   function Target_Key (T : Platforms.Targets) return String
   is (TOML_Keys.Target);
   function Tomify is new TOML_Adapters.Tomify (Platforms.Targets);
   package Targets is new Values (Platforms.Targets,
                                  Platforms.Targets'Image,
                                  Platforms.Targets'Image,
                                  Target_Key,
                                  Tomify);

   function Version_Key (V : Platforms.Versions) return String
   is (raise Unimplemented); -- Probably due to be deprecated?
   function Tomify is new TOML_Adapters.Tomify (Platforms.Versions);
   package Versions is new Values (Platforms.Versions,
                                   Platforms.Versions'Image,
                                   Platforms.Versions'Image,
                                   Version_Key,
                                   Tomify);

   function Word_Size_Key (WS : Platforms.Word_Sizes) return String
   is (TOML_Keys.Word_Size);
   function Tomify is new TOML_Adapters.Tomify (Platforms.Word_Sizes);
   package Word_Sizes is new Values (Platforms.Word_Sizes,
                                     Platforms.Word_Sizes'Image,
                                     Platforms.Word_Sizes'Image,
                                     Word_Size_Key,
                                     Tomify);

   pragma Warnings (On);

   function Compiler_Is (C : Platforms.Compilers) return Vector
   renames Compilers.New_Vector;

   function Distribution_Is (D : Platforms.Distributions) return Vector
   renames Distributions.New_Vector;

   function System_Is (OS : Platforms.Operating_Systems) return Vector
   renames Operating_Systems.New_Vector;

   function Target_Is (T : Platforms.Targets) return Vector
   renames Targets.New_Vector;

   function Version_Is (V : Platforms.Versions) return Vector
   renames Versions.New_Vector;

   function Word_Size_Is (V : Platforms.Word_Sizes) return Vector
   renames Word_Sizes.New_Vector;

end Alire.Properties.Platform;
