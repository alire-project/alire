with Alire.Index.GNATCOLL;

package Alire.Index.XStrings is

   function Project is new Catalogued_Project
     ("Renaming of gnatcoll.strings without further dependencies");

   Repo : constant URL := "https://github.com/alire-project/xstrings";

   Base : constant Release :=
            Project.Unreleased
              (Dependencies =>
                 GNATCOLL.Strings.V_20180425.Within_Major or
                 GNATCOLL.Slim.V_20180425.Within_Major,

               Properties =>
                 Author     ("alejandro@mosteo.com") and
                 License    (GPL_3_0));

   package V_1_0_0 is new Project_Release
     (Base
      .Replacing (Git (Repo, "827d9108fbc873299016e924815fe2dd8af8071d")));

end Alire.Index.XStrings;
