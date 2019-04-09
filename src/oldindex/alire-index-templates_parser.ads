with Alire.Index.XMLAda;

package Alire.Index.Templates_Parser is

   function Project is new Catalogued_Project
     ("Templates Parser: File generation from templates in Ada");

   Repo : constant URL := "https://github.com/AdaCore/templates-parser.git";

   Base : constant Release := Project.Unreleased
     (Dependencies =>
        XMLAda.V_18_2.Within_Major,

      Properties =>
        GPR_Scenario ("PRJ_BUILD", "Debug" or "Release") and
        GPR_Scenario ("TP_TASKING", "No_Tasking " or  "Standard_Tasking") and
        GPR_Scenario ("TP_XMLADA", "Installed " or  "Disabled") and
        GPR_Scenario ("LIBRARY_TYPE", "static" or  "relocatable") and

        Maintainer ("AdaCore") and
        Website    ("https://github.com/AdaCore/templates-parser") and
        License    (GPL_3_0),

      Private_Properties =>
        GPR_External ("TP_XMLADA", "Installed") and
        Action_Run (Post_Fetch, "cp config/tp_xmlada_installed.gpr tp_xmlada.gpr")
     );

   package V_18_2 is new Project_Release
     (Base
      .Replacing
        (Git (Repo, "cfb146506fa2fa276e935244021d44e0d834c342")));

end Alire.Index.Templates_Parser;
