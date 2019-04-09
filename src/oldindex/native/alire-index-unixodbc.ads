package Alire.Index.UnixODBC is

   function Project is new Catalogued_Project
     ("Open Database Connectivity drivers for UNIX platforms");

   V_2_3 : constant Release :=
             Project.Register
               (V ("2.3"),
                Native ((Debian | Ubuntu => Packaged_As ("unixodbc-dev"),
                         others          => Unavailable)),
                Properties =>
                  Website ("www.unixodbc.org"));

end Alire.Index.UnixODBC;
