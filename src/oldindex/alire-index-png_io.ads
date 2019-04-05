with Alire.Index.Zlib_Ada;

package Alire.Index.PNG_IO is

   function Project is new Catalogued_Project
     ("Ada95 coder/decoder for Portable Network Graphics");

   Repo : constant URL := "https://github.com/alire-project/png_io.git";

   Base : constant Release :=
            Project.Unreleased
              (Properties =>
                 Executable ("png_chunks") and
                 Executable ("png_compare") and
                 Executable ("png_dump") and
                 Executable ("png_properties") and
                 Executable ("png_test") and

                 Author     ("Steve Sangwine") and
                 Website    ("http://png-io.sourceforge.net/") and
                 License    (GPL_3_0));

   package V_4_6_0 is new Project_Release
     (Base
      .Replacing (Git (Repo, "bb31fe1c1566e13339dec26ae359564364c00999"))
      .Extending (Dependencies => Zlib_Ada.V_1_3_0.Within_Major));

end Alire.Index.PNG_IO;
