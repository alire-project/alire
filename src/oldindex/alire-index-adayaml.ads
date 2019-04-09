with Alire.Index.AUnit;
with Alire.Index.Simple_Components;

package Alire.Index.AdaYaml is

   function Project is
     new Catalogued_Project ("Experimental YAML 1.3 implementation in Ada");

   Prj_Repo       : constant URL    := "https://github.com/yaml/AdaYaml.git";
   Prj_Author     : constant String := "Felix Krause";
   Prj_Website    : constant URL    := "https://ada.yaml.io/";

   Base : constant Release :=
            Project.Unreleased
              (Dependencies =>
                 AUnit.Project >= AUnit.V_2017,

               Properties =>
                 Project_File ("yaml.gpr") and
                 Project_File ("yaml-utils.gpr") and

                 GPR_Scenario ("Mode", "debug" or "release") and

                 Author     (Prj_Author) and
                 Website    (Prj_Website) and
                 License    (MIT),

               Private_Properties =>
                 Project_File ("yaml-tests.gpr"));

   package V_0_3 is new Project_Release
     (Base
      .Replacing (Git (Prj_Repo, "2017a7c2523499c03b8d7fe06546a5a8bae6476d"))
      .Extending (Properties =>
                    Project_File ("yaml-annotation_processor.gpr"),
                  Private_Properties =>
                    Executable ("yaml-dumping_tests-harness") and
                    Executable ("yaml-loading_tests-harness") and
                    Executable ("yaml-transformation_tests-harness")));

   package V_0_2 is new Project_Release
     (Base
      .Replacing (Git (Prj_Repo, "0264b03fd92eeedfe3e2713ed1da3f0d255c1727"))
      .Extending (Private_Properties =>
                    Executable ("yaml-lexer-harness") and
                    Executable ("yaml-parser-harness")));

   ------------
   -- Server --
   ------------

   package Server is

      function Project is new Catalogued_Project
        ("Experimental YAML 1.3 implementation in Ada (server component)");

      V_0_3 : constant Release :=
                Project.Register
                  (AdaYaml.V_0_3.Release
                   .Extending
                     (Properties   =>
                        Project_File ("yaml-server.gpr") and
                        Executable ("yaml-server"),

                      Dependencies =>
                        Simple_Components.Connections.V_4_27.Within_Major));

   end Server;

end Alire.Index.AdaYaml;
