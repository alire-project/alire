with Alire.Index.Make;

package Alire.Index.Whitakers_Words is

   function Project is
     new Catalogued_Project ("William Whitaker's WORDS, a Latin dictionary");

   Prj_Repo       : constant URL    := "https://github.com/mk270/whitakers-words.git";
   Prj_Author     : constant String := "William A. Whitaker";
   Prj_Maintainer : constant String := "Martin Keegan";
   Prj_Website    : constant URL    := "http://mk270.github.io/whitakers-words/";

   V_2017_09_10 : constant Release :=
                    Project.Register
                      (V ("2017.09.10"),
                       Git (Prj_Repo, "27be95b8a06d7b22c0600c824cf929ab43efcf25"),
                       Dependencies =>
                         Make.Project.Current,

                       Properties         =>
                         Project_File ("words.gpr") and

                         Executable ("words") and

                         Author     (Prj_Author) and
                         Maintainer (Prj_Maintainer) and
                         Website    (Prj_Website) and
                         License    (Public_Domain),

                       Private_Properties =>
                         Action_Run (Post_Compile, "make"),

                       Available_When     =>
                         Compiler > GNAT_FSF_7_3_Or_Newer
                         -- bug with SAL library failing binding
                      );

end Alire.Index.Whitakers_Words;
