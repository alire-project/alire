with Alire.Index.Make;

package Alire.Index.ASIS is

   function Project is
     new Catalogued_Project
       ("Ada Semantic Interface Specification or ASIS (library part)");

   Src_2018 : constant URL :=
     "http://mirrors.cdn.adacore.com/art/5b0819e0c7a447df26c27ab8";
   Src_2017 : constant URL :=
     "http://mirrors.cdn.adacore.com/art/591c45e2c7a447af2deecffb";

   Common_Dependencies : constant Release_Dependencies :=
                           Make.Project.Current;

   Common_Properties : constant Release_Properties :=
     Maintainer ("AdaCore") and
     Website ("https://www.adacore.com/download/more") and
     License (GPL_3_0);

   V_2018 : constant Release :=
              Project.Register
                (V ("2018"),
                 Source_Archive
                   (Src_2018,
                    "asis-gpl-2018-20180524-src.tar.gz"),

                 Dependencies =>
                   Common_Dependencies,

                 Properties         =>
                   Project_File ("asis-gpl-2018-src/asis.gpr") and
                   Common_Properties,

                 Private_Properties =>
                   Action_Run
                     (Post_Fetch, "make setup-snames -C asis-gpl-2018-src"),

                 Available_When     =>
                    Compiler = GNAT_Community_2018
                );

   V_2017 : constant Release :=
              Project.Register
                (V ("2017"),
                 Source_Archive
                   (Src_2017,
                    "asis-gpl-2017-src.tar.gz"),

                 Dependencies =>
                   Common_Dependencies,

                 Properties         =>
                   Project_File ("asis-gpl-2017-src/asis.gpr") and
                   Common_Properties,

                 Private_Properties =>
                   Action_Run
                     (Post_Fetch, "make setup-snames -C asis-gpl-2017-src"),

                 Available_When     =>
                    Compiler = GNAT_Community_2018
                );

end Alire.Index.ASIS;
