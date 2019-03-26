package Alire.Index.LibX11 is

   function Project is new Catalogued_Project ("X11 client-side library");

   V_2 : constant Release :=
           Project.Register
             (V ("2"),
              Native ((Debian | Ubuntu => Packaged_As ("libx11-dev"),
                       others          => Unavailable)));

end Alire.Index.LibX11;
