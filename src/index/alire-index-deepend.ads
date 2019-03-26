package Alire.Index.Deepend is

   function Project is new Catalogued_Project
     ("A suite of dynamic storage pools with subpool capabilities");

   Repo : constant URL := "https://github.com/alire-project/deepend.git";

   Base : constant Release :=
            Project.Unreleased
              (Properties =>
                 Author     ("Brad Moore") and
                 Website    ("https://sourceforge.net/projects/deepend/") and
                 License    (GPL_2_0));

   package V_3_9 is new Project_Release
     (Base
      .Replacing
        (Git (Repo, "4491dcf36a9264110f9cd3876cdcd476f1fbf09d"))
      .Extending
        (Properties =>
             GPR_Scenario ("Ada_Mode", "95" or "2005" or "2012")));

end Alire.Index.Deepend;
