package Alire.Index.C_Strings is

   function Project is new Catalogued_Project
     ("Convenience subprograms to interact with C strings");

   Repo : constant URL := "https://github.com/mosteo/cstrings.git";

   Base : constant Release :=
            Project.Unreleased
              (Properties =>
                 Author     ("alejandro@mosteo.com") and
                 License    (LGPL_3_0));

   package V_1_0_0 is new Project_Release
     (Base
      .Replacing (Git (Repo, "2bd507ca150d2c58e13ae98443614a3c85330cb5")));

end Alire.Index.C_Strings;
