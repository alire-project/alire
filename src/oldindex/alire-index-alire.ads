with Alire.Index.AAA;
with Alire.Index.Semantic_Versioning;
with Alire.Index.Simple_Logging;

package Alire.Index.Alire is

   function Project is
     new Catalogued_Project ("Alire project catalog and support files");

   Repo : constant URL := "https://github.com/alire-project/alire.git";

   Base : constant Release :=
           Project.Unreleased
             (V ("0.0"),
              No_Origin,
              Properties =>
                Author ("Alejandro R. Mosteo") and
                License (GPL_3_0));

   package V_0_6 is new Project_Release
     (Base
      .Replacing (Origin =>
                       Git (Repo, "f418890a85f421b20ad00f1f52259c122f883aca"))
      .Extending (Dependencies =>
                       AAA.V_1_0_0.Within_Major and
                       Semantic_Versioning.V_0_3_2.Within_Minor and
                       Simple_Logging.V_1_0.Within_Major));

   package V_0_5 is new Project_Release
     (Base
      .Replacing (Origin =>
                       Git (Repo, "ff4f75f938a22173b8296efb21e112eb63865882"))
      .Extending (Dependencies =>
                       Semantic_Versioning.V_0_3_2.Within_Minor and
                       Simple_Logging.V_1_0.Within_Major));

   --  Old-style early releases

   V_0_4 : constant Release :=
               Project.Register
                 (Base
                  .Upgrading
                    (V ("0.4"),
                     Git (Repo, "219cdcbc5f26efca331400582026c6377ef0f794"))
                  .Extending
                    (Dependencies =>
                       Semantic_Versioning.V_0_3_1.Within_Minor and
                       Simple_Logging.V_1_0.Within_Major));

   V_0_2 : constant Release :=
               Project.Register
                 (Base
                  .Upgrading
                    (V ("0.2"),
                     Git (Repo, "5ba81ba33dfeb184b2e644ef2996200b5fdd6ae4"))
                  .Extending
                    (Dependencies =>
                       Semantic_Versioning.V_0_3.Within_Minor and
                       Simple_Logging.V_1_0.Within_Major));

   V_0_1_2 : constant Release :=
               Project.Register
                 (Base
                  .Upgrading
                    (V ("0.1.2"),
                     Git (Repo, "e2dee2e147ae9e4d666567b53b108cbe61bc06e8"))
                  .Extending
                    (Dependencies =>
                       Semantic_Versioning.V_0_1_2.Within_Minor and
                       Simple_Logging.V_1_0.Within_Major));

   function Example_Project return Catalog_Entry renames Project;
   function Elite_Dangerous return Catalog_Entry renames Project;
   function Half_Life       return Catalog_Entry renames Project;
   function Star_Citizen    return Catalog_Entry renames Project;
   function Windows_3000    return Catalog_Entry renames Project;

   Syntax_Example : constant Release :=
                      Example_Project.Unreleased
                        (V ("0.0.1"),
                         Origins.New_Filesystem ("/alire"),
                         Notes => "Mock release with examples of complex conditions",
                         Dependencies       =>
                           Half_Life >= "3.0" and -- unconditional
                             On_Condition            -- conditional
                               (Operating_System = GNU_Linux,
                                  When_True  => Elite_Dangerous >= "2.0" and Star_Citizen >= V ("3.0"), -- Wish...
                                  When_False => Windows_3000 > V ("1.0")) and
                           (Star_Citizen >= "4.0" or Half_Life >= "3.0"),
                         -- Chained preferences, takes first available

                         Private_Properties => -- These are only interesting to alr, not users
                           GPR_External ("Profile", "False"),
                         --  Sample extra params for build

                         Properties         =>
                           GPR_Scenario ("Build", "Debug" or "Release") and
                           GPR_Free_Scenario ("Path_To_Something") and
                         --  Known scenario variables

                           Project_File ("scenarios/catastrophical.gpr") and
                         --  Way to specify a project file not named like the project
                         --  Path separators are always "/" and internally converted to native ones

                           On_Condition
                             (Operating_System = Windows,
                              Project_File ("project_win.gpr")) and
                           On_Condition
                             (Operating_System = GNU_Linux,
                              On_Condition (Distribution = Ubuntu, -- Nested conditions
                                Project_File ("project_ubuntu.gpr"))) and
                         --  Conditional project file

                           On_Condition
                             (Operating_System = GNU_Linux,
                              Comment ("Long life the penguin")) and
                           --  Conditions on operating system

                           Case_Operating_System_Is
                             ((GNU_Linux => Comment ("Longerer life to the penguin"),
                               OSX       => Comment ("Oh shiny!"),
                               others    => Comment ("Pick your poison"))) and
                           --  Also as Case-like statements

                           On_Condition
                             (Compiler = GNAT_Unknown, -- /= also works
                              Comment ("Never saw that compiler") and Comment ("But I would like to")) and
                         --  Conditions on compiler version

                           On_Condition
                             (Distro_Release = Ubuntu_Bionic,
                              When_True  => Comment ("Living on the edge"),
                              When_False => Comment ("I am a rock")) and
                         --  Conditions on distribution release

                           Comment ("Tell me about your mother") and
                           Website ("http://www.www.www"),
                         --  Unconditional properties

                         Available_When     => -- Impossible mix
                           (Operating_System = Windows and Operating_System /= GNU_Linux) or
                           (Compiler = GNAT_Unknown and Compiler /= GNAT_Unknown));

--     package Experimental is
--
--        function Project is new Catalogued_Project ("Experimental packages");
--
--        Base : constant Release := Project.Unreleased;
--
--        package V1 is new Project_Release (Base);
--        package V2 is new Project_Release (Base);
--        package V3 is new Project_Release (Base);
--
--        package Greedy_Breaker is
--
--           function Project is new Catalogued_Project
--             ("Dependency too complex for the greedy solver");
--
--           R1 : constant Release := Project.Register
--             (V ("1"),
--              No_Origin,
--              Dependencies =>
--                (V2.This_Version      -- This causes V2 to be chosen greedily
--                 or V3.This_Version)  -- This will never be attempted
--                and V3.This_Version); -- And thus this will never be met
--
--           R2 : constant Release := Project.Register
--             (V ("2"),
--              No_Origin,
--              Dependencies =>
--                (V1.This_Version or V2.This_Version or V3.This_Version));
--        end Greedy_Breaker;
--
--     end Experimental;

end Alire.Index.Alire;
