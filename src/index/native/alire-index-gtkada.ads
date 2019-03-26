with Alire.Index.GNAT;

package Alire.Index.GtkAda is

   function Project is new Catalogued_Project ("Ada binding for the GTK+ GUI");

   V_17 : constant Release :=
              Project.Register
                (V ("17"),
                 Native ((Debian | Ubuntu => Packaged_As ("libgtkada16.1.0-dev"),
                          others          => Unavailable)),

                 Dependencies =>
                   GNAT.Project.Current
                );

end Alire.Index.GtkAda;
