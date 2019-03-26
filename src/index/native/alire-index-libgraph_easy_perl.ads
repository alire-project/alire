package Alire.Index.Libgraph_Easy_Perl is

   function Project is new Catalogued_Project ("Drawing of ASCII graphs");

   V_Rolling : constant Release :=
                 Project.Register
                   (V ("0.0-rolling"),
                    Native ((Debian | Ubuntu => Packaged_As ("libgraph-easy-perl"),
                             others          => Unavailable)));

end Alire.Index.Libgraph_Easy_Perl;
