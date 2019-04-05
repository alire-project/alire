package Alire.Index.AAA is

   function Project is new Catalogued_Project
     ("Alex's Ada Assortment (of miscellaneous utilities)");

   Repo : constant URL := "https://github.com/mosteo/aaa.git";

   Base : constant Release :=
            Project.Unreleased
              (Properties =>
                 Author     ("alejandro@mosteo.com") and
                 License    (LGPL_3_0));

   package V_1_0_0 is new Project_Release
     (Base
      .Replacing (Git (Repo, "3b263b6cb858c1ca0043674d21ae3e618c4026e2")));

end Alire.Index.AAA;
