with Alire.Index.GtkAda;

package Alire.Index.Eagle_Lander is

   function Project is
     new Catalogued_Project ("Apollo 11 lunar lander simulator (Ada/Gtk/Cairo)");

   Prj_Repo    : constant URL    := "https://github.com/alire-project/eagle-lander.git";
   Prj_Author  : constant String := "Fabien Chouteau";
   Prj_Website : constant URL    := "https://blog.adacore.com/make-with-ada-the-eagle-has-landed";

   V_1_0 : constant Release :=
             Project.Register
               (V ("1.0"),
                Git (Prj_Repo, "5a3bcc61eff4d60d2b741add7841410ce539d0b8"),

                Dependencies       =>
                  GtkAda.V_17.Within_Major,

                Properties         =>
                  Project_File ("lunar_lander.gpr") and

                  Author     (Prj_Author) and
                  Website    (Prj_Website) and
                  License    (GPL_3_0)
               );

end Alire.Index.Eagle_Lander;
