package Alire.Index.GLUT is

   function Project is new Catalogued_Project ("OpenGL Utility Toolkit");

   V_2_8_1 : constant Release :=
               Project.Register
                 (V ("2.8.1-3"),
                  Native ((Debian | Ubuntu => Packaged_As ("freeglut3-dev"),
                           others          => Unavailable)));

end Alire.Index.GLUT;
