package Alire.Index.RxAda is

   Name : constant Project_Name := "rxada";
   Repo : constant URL          := "https://bitbucket.org/amosteo/rxada";

   Desc : constant Project_Description := "RxAda port of the Rx framework";

   V_0_1_0  : constant Release :=
                Register (Name,
                          V ("0.1.0"),
                          Desc,
                          Hg (Repo, "361d4e2ab20a7dcca007e31bf7094d57b13fee6b"),
                          Properties =>

                            Executable ("rx-examples-basic") and
                            Executable ("rx-examples-minimal") and
                            Executable ("rx-examples-tests") and
                            Executable ("rx-examples-threading") and

                            License (LGPL_3_0) and
                            Maintainer ("alejandro@mosteo.com") and
                            Website    (Repo));

end Alire.Index.RxAda;
