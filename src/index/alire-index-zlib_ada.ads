with Alire.Index.Zlib;

package Alire.Index.ZLib_Ada is

   function Project is new Catalogued_Project
     ("ZLib for Ada thick binding");

   Repo : constant URL := "https://github.com/alire-project/zlib-ada.git";

   Base : constant Release :=
            Project.Unreleased
              (Properties =>
                 Project_File ("zlib.gpr") and
                 Executable ("buffer_demo") and
                 Executable ("mtest") and
                 Executable ("read") and
                 Executable ("test") and

                 Author     ("Dmitriy Anisimkov") and
                 Website    ("http://zlib-ada.sourceforge.net/") and
                 License    (Unknown));

   package V_1_3_0 is new Project_Release
     (Base
      .Replacing (Git (Repo, "2bacba46c2d4d753c300848fe8134ee150078167"))
      .Extending (Dependencies => Zlib.V_1_2.Within_Major));

end Alire.Index.ZLib_Ada;
