with Alire.Index.GLUT;

package Alire.Index.Globe_3D is

   function Project is
     new Catalogued_Project ("GL Object Based Engine for 3D in Ada");

   Prj_Repo    : constant URL    := "https://github.com/svn2github/GLOBE_3D.git";
   Prj_Author  : constant String := "Gautier de Montmollin";
   Prj_Website : constant URL    := "https://globe3d.sourceforge.io/";

   V_20180111 : constant Release :=
                  Project.Register
                    (V ("20180111"),
                     Git (Prj_Repo, "93f7185130e2fb0db7f1f7e67eaf1b6ca561d651"),

                     Dependencies       =>
                       GLUT.V_2_8_1.Within_Major,

                     Properties         =>
                       Project_File ("globe_3d.gpr") and

                       GPR_Scenario ("OS_Kind", "linux" or "macosx" or "win32") and

                       Executable ("globe_3d_demo") and
                       Executable ("launch_armada") and
                       Executable ("launch_multi_window") and
                       Executable ("launch_sprite_demo") and
                       Executable ("mini") and

                       Author     (Prj_Author) and
                       Website    (Prj_Website) and
                       License    (Unknown),

                     Private_Properties =>
                       Project_File ("demo/culler/armada/armada.gpr") and
                       Project_File ("demo/globe_3d_demos.gpr") and
                       Project_File ("demo/multi_window/multi_window.gpr") and
                       Project_File ("demo/sprite/sprite_demo.gpr") and

                       On_Condition
                         (Operating_System = GNU_Linux,
                          GPR_External ("OS_Kind", "linux")),

                     Available_When     =>
                       Operating_System = GNU_Linux
                       -- It's available in more platforms, but still untested and with unknown dependencies
                    );

end Alire.Index.Globe_3D;
