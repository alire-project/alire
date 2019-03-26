package Alire.Index.Libglfw3 is

   function Project is new Catalogued_Project
     ("Portable library for OpenGL, window and input");

   V_3 : constant Release :=
           Project.Register
             (V ("3"),
              Native ((Debian | Ubuntu => Packaged_As ("libglfw3-dev"),
                       others          => Unavailable)));

end Alire.Index.Libglfw3;
