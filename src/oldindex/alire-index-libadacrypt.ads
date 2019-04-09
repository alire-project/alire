with Alire.Index.AUnit;

package Alire.Index.Libadacrypt is

   function Project is
     new Catalogued_Project ("A crypto library for Ada with a nice API");

   Prj_Repo   : constant URL    := "https://github.com/alire-project/Ada-Crypto-Library.git";
   Prj_Author : constant String := "Christian Forler";

   V_0_8_7 : constant Release :=
               Project.Register
                 (V ("0.8.7"),
                  Git (Prj_Repo, "33d15283abbc6d8a51d717de2bd822e026710c0d"),

                  Dependencies       =>
                    AUnit.V_2017.Within_Major,

                  Properties         =>
                    Project_File ("libadacrypt.gpr") and

                    GPR_Scenario ("system", "unix" or "windows") and
                    GPR_Scenario ("mode", "debug" or "release") and

                    Executable ("test-asymmetric_ciphers") and
                    Executable ("test-big_number") and
                    Executable ("test-kdf") and
                    Executable ("test-symmetric_ciphers") and
                    Executable ("test-tests") and

                    Author     (Prj_Author) and
                    License    (GMGPL_2_0) and
                    License    (GMGPL_3_0),

                  Private_Properties =>
                    Project_File ("acltest.gpr") and

                    On_Condition
                      (Operating_System = GNU_Linux,
                       GPR_External ("system", "unix")) and
                    On_Condition
                      (Operating_System = Windows,
                       GPR_External ("system", "windows")),

                  Available_When     =>
                    Compiler > GNAT_FSF_7_3_Or_Newer
                    --  It fails self-tests; might be a spureous warning
                 );

end Alire.Index.Libadacrypt;
