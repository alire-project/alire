package Alire.Index.PDF_Out is

   function Project is new Catalogued_Project
     ("Standalone, portable package for producing dynamically PDF documents");

   Repo : constant URL := "https://github.com/svn2github/pdf_out.git";

   Base : constant Release :=
            Project.Unreleased
              (Properties =>
                 Author     ("Gautier de Montmollin") and
                 Website    ("https://sourceforge.net/projects/apdf") and
                 License    (MIT));

   package V_1_0_0_RC4 is new Project_Release
     (Base
      .Replacing (Git (Repo, "9f2d3adbc453e03fce88646d80d2ed9c9a58b32b"))
      .Extending
        (Properties =>
             Project_File ("pdf_out_gnat.gpr") and

             GPR_Scenario ("Build_Mode", "Debug" or "Fast" or "Check_95") and

             Executable ("img2pdf") and
             Executable ("page_test") and
             Executable ("pdf_out_demo") and
             Executable ("validation_test")));

end Alire.Index.PDF_Out;
