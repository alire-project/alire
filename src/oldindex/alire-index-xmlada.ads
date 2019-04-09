package Alire.Index.XMLAda is

   function Project is new Catalogued_Project ("The XML/Ada toolkit");

   Repo : constant URL := "https://github.com/AdaCore/xmlada.git";

   Base : constant Release := Project.Unreleased
     (Properties =>
        GPR_Scenario ("LIBRARY_TYPE", "relocatable" or "static" or "static-pic") and
        GPR_Scenario ("BUILD", "distrib" or  "Debug" or "Production" or "profile" or "coverage" or "nochecks") and

        Project_File ("distrib/xmlada.gpr") and

        Maintainer ("AdaCore") and
        Website    ("https://github.com/AdaCore/xmlada") and
        License    (GPL_3_0),

      Private_Properties =>
        Project_File ("dom/xmlada_dom.gpr") and
        Project_File ("input_sources/xmlada_input.gpr") and
        Project_File ("sax/xmlada_sax.gpr") and
        Project_File ("schema/xmlada_schema.gpr") and
        Project_File ("unicode/xmlada_unicode.gpr") and

        Action_Run (Post_Fetch, "sh configure") and
        Action_Run (Post_Fetch, "rm -f xmlada.gpr"), -- Else it conflicts

      Available_When =>
        Operating_System /= Windows);

   package V_18_2 is new Project_Release
     (Base
      .Replacing
        (Git (Repo, "5c3c4a1621a970849601a9df36423d8974c13dec")));

end Alire.Index.XMLAda;
