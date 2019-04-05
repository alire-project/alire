package Alire.Index.Make is

   function Project is new Catalogued_Project
     ("Utility for directing compilation");

   V_Native : constant Release :=
                Project.Register (V ("0"),
                                  Native ((Debian | Ubuntu => Packaged_As ("make"),
                                           others          => Unavailable)));

end Alire.Index.Make;
