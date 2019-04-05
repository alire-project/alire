with Alire.Index.XML_EZ_Out;

package Alire.Index.AJUnitGen is

   function Project is new Catalogued_Project
     ("Generator of JUnit-compatible XML reports");

   Repo : constant URL := "https://github.com/mosteo/ajunitgen.git";

   Base : constant Release :=
            Project.Unreleased
              (Properties =>
                 Author     ("Alejandro R. Mosteo") and
                 License    (LGPL_3_0),
               Dependencies =>
                 XML_EZ_Out.V_1_6.Within_Major);

   package V_1_0_0 is new Project_Release
     (Base
      .Replacing (Git (Repo, "d2d110f92b8175ace6668dfdb639ea10ece5a822")));

end Alire.Index.AJUnitGen;
