package Alire.Index.LibSDL2 is

   function Project is new Catalogued_Project
     ("Simple DirectMedia Layer development files");

   V_2 : constant Release :=
               Project.Register
                 (V ("2"),
                  Native ((Debian | Ubuntu => Packaged_As ("libsdl2-dev"),
                           others          => Unavailable)));

   package Image is

      function Project is new Catalogued_Project
        ("Image loading library for Simple DirectMedia Layer 2");

      V_2 : constant Release :=
                        Project.Register
                          (V ("2"),
                           Native ((Debian | Ubuntu => Packaged_As ("libsdl2-image-dev"),
                                    others          => Unavailable)));

   end Image;

   package TTF is

      function Project is new Catalogued_Project
        ("TrueType Font library for Simple DirectMedia Layer 2");

      V_2 : constant Release :=
                      Project.Register
                        (V ("2"),
                         Native ((Debian | Ubuntu => Packaged_As ("libsdl2-ttf-dev"),
                                  others          => Unavailable)));

   end TTF;

end Alire.Index.LibSDL2;
