package Alire.Index.AUnit is

   function Project is
     new Catalogued_Project ("Ada unit test framework");

   Prj_Repo : constant URL := "https://github.com/alire-project/libaunit.git";

   Common_Properties : constant Release_Properties :=
                           Maintainer ("AdaCore") and
                           Website ("https://www.adacore.com/download/more") and
                           License (GPL_3_0);

   V_2017 : constant Release :=
              Project.Register
                (V ("2017"),
                 Git (Prj_Repo, "b66a41ceb35bfc81b9345655c5f46317a57de3b4"),
                 Properties         =>
                   GPR_Scenario ("RUNTIME",
                     "full" or "zfp" or "ravenscar" or "ravenscar-cert" or "cert") and

                   Project_File ("aunit.gpr") and

                   Executable ("aunit_harness") and
                   Executable ("run-ppc-elf") and
                   Executable ("test_liskov") and
                   Executable ("test_calculator") and
                   Executable ("test_math") and

                   Common_Properties,

                 Private_Properties =>
                   Project_File ("test/aunit_tests.gpr") and

                   Project_File ("examples/calculator/harness.gpr") and
                   Project_File ("examples/calculator/tested_lib/testlib.gpr") and
                   Project_File ("examples/failures/harness.gpr") and
                   Project_File ("examples/failures/tested_lib/testlib.gpr") and
                   Project_File ("examples/liskov/harness.gpr") and
                   Project_File ("examples/liskov/tested_lib/testlib.gpr") and
                   Project_File ("examples/simple_test/harness.gpr") and
                   Project_File ("examples/simple_test/tested_lib/testlib.gpr") and
                   Project_File ("examples/test_caller/harness/harness.gpr") and
                   Project_File ("examples/test_caller/tested_lib/testlib.gpr") and
                   Project_File ("examples/test_fixture/harness.gpr") and
                   Project_File ("examples/test_fixture/tested_lib/testlib.gpr")
                );

end Alire.Index.AUnit;
