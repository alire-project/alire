with Alire.Platforms;

package Alire.Properties.Platform with Preelaborate is

   --  The following packages declare types used by requisites so they have to be public  --

   package Compilers is new Values (Platforms.Compilers,
                                    Platforms.Compilers'IMage);

   package Distributions is new Values (Platforms.Distributions,
                                        Platforms.Distributions'Image);

   package Operating_Systems is new Values (Platforms.Operating_Systems,
                                            Platforms.Operating_Systems'Image);

   package Targets is new Values (Platforms.Targets,
                                  Platforms.Targets'Image);

   package Versions is new Values (Platforms.Versions,
                                   Platforms.Versions'Image);

   package Word_Sizes is new Values (Platforms.Word_Sizes,
                                     Platforms.Word_Sizes'Image);


   function Compiler_Is (C : Platforms.Compilers) return Vector renames Compilers.New_Vector;

   function Distribution_Is (D : Platforms.Distributions) return Vector renames Distributions.New_Vector;

   function System_Is (OS : Platforms.Operating_Systems) return Vector renames Operating_Systems.New_Vector;

   function Target_Is (T : Platforms.Targets) return Vector renames Targets.New_Vector;

   function Version_Is (V : Platforms.Versions) return Vector renames Versions.New_Vector;

   function Word_Size_Is (V : Platforms.Word_Sizes) return Vector renames Word_Sizes.New_Vector;

end Alire.Properties.Platform;
