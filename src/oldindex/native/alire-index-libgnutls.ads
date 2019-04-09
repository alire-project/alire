package Alire.Index.LibGNUTLS is

   function Project is new Catalogued_Project ("GNU TLS library");

   V_3_5_8 : constant Release :=
               Project.Register
                 (V ("3.5.8"),
                  Native ((Debian | Ubuntu => Packaged_As ("libgnutls28-dev"),
                           others           => Unavailable)));

end Alire.Index.LibGNUTLS;
