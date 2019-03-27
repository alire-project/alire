package Alire.Index.Liblua is

   function Project is new Catalogued_Project
     ("Development files for the Lua language");

   V_5_3 : constant Release :=
             Project.Register
               (V ("5.3"),
                Native ((Debian | Ubuntu => Packaged_As ("liblua5.3-dev"),
                         others           => Unavailable)));

end Alire.Index.Liblua;
