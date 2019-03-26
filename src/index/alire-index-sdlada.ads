with Alire.Index.LibSDL2;

package Alire.Index.SDLAda is

   function Project is
     new Catalogued_Project ("Ada 2012 bindings to SDL 2");

   Prj_Repo   : constant URL    := "https://github.com/alire-project/sdlada.git";
   Prj_Author : constant String := "Luke A. Guest";

   V_2_3_1 : constant Release :=
               Project.Register
                 (V ("2.3.1"),
                  Git (Prj_Repo, "570232193facb90a58f67aadac93df9dfae8bcd4"),

                  Dependencies       =>
                    LibSDL2.V_2.Within_Major and
                    LibSDL2.Image.V_2.Within_Major and
                    LibSDL2.TTF.V_2.Within_Major,

                  Properties         =>
                    Project_File ("build/gnat/sdlada.gpr") and

                    GPR_Scenario ("SDL_MODE", "debug" or "release") and
                    GPR_Scenario ("SDL_PLATFORM", "linux" or "bsd" or "windows" or "macosx" or "ios" or "android") and

                    Author     (Prj_Author) and
                    License    (Zlib),

                  Private_Properties =>
                    Project_File ("build/gnat/tests.gpr") and
                    -- Project_File ("build/gnat/tests_image.gpr") and
                    Project_File ("build/gnat/test_maths_build.gpr") and
                    -- Project_File ("build/gnat/tools.gpr") and
                    -- Project_File ("build/gnat/unit_tests.gpr") and

                    Executable ("clipboard") and
                    Executable ("error") and
                    Executable ("libraries") and
                    Executable ("load_surface") and
                    Executable ("platform") and
                    Executable ("rwops") and
                    Executable ("stream") and
                    Executable ("stream2") and
                    Executable ("surface") and
                    Executable ("test") and
                    Executable ("version") and

                    GPR_External ("SDL_MODE", "release") and

                    On_Condition
                      (Operating_System = GNU_Linux,
                       GPR_External ("SDL_PLATFORM", "linux")),

                  Available_When     =>
                    Operating_System = GNU_Linux
                 );

end Alire.Index.SDLAda;
