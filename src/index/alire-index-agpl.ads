with Alire.Index.Libgsl;

package Alire.Index.Agpl is

   function Project is new Catalogued_Project
     ("Ada General Purpose Library with a robotics flavor");

   Repo : constant URL := "https://github.com/mosteo/agpl.git";

   Base : constant Release :=
            Project.Unreleased
              (Dependencies =>
                 Libgsl.Project.Current,
               Properties =>
                 GPR_Scenario ("Agpl_Include_Concorde", "True" or "False") and
                 GPR_Scenario ("Agpl_Include_Db",       "True" or "False") and
                 GPR_Scenario ("Agpl_Include_Boost",    "True" or "False") and
                 GPR_Scenario ("Agpl_Include_PngIO",    "True" or "False") and

                 Author     ("alejandro@mosteo.com") and
                 License    (LGPL_3_0));

   package V_1_0_0 is new Project_Release
     (Base
      .Replacing (Git (Repo, "00ac879f8d049171206699da39ac01f126b196e0")));

end Alire.Index.Agpl;
