package Alire.Index.Mandelbrot_ASCII is

   function Project is new Catalogued_Project
     ("Mandelbrot renderer using Unicode glyphs");

   Repo : constant URL := "https://github.com/mosteo/mandelbrot_ascii.git";

   Base : constant Release :=
            Project.Unreleased
              (Properties =>
                 Author     ("David Given") and
                 Maintainer ("Alejandro R. Mosteo") and
                 License    (Unknown));

   package V_1 is new Project_Release
     (Base.Replacing (Git (Repo, "53efca17cdcc2d42c3a87e4344fa782fbeac906e")));

end Alire.Index.Mandelbrot_ASCII;
