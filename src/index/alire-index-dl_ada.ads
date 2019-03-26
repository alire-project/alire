with Alire.Index.C_Strings;

package Alire.Index.DL_Ada is

   function Project is new Catalogued_Project ("Partial binding to libdl");

   Repo : constant URL := "https://github.com/mosteo/dl-ada.git";

   Base : constant Release :=
            Project.Unreleased
              (Dependencies =>
                 C_Strings.V_1_0_0.Within_Major,
               Properties   =>
                 Project_File ("dl.gpr") and
                 Author     ("alejandro@mosteo.com") and
                 License    (LGPL_3_0));

   package V_1_0_0 is new Project_Release
     (Base
      .Replacing (Git (Repo, "6e86754884e58908777070798645475fd47a4d0a")));

end Alire.Index.DL_Ada;
