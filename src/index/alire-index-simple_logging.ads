package Alire.Index.Simple_Logging is

   function Project is new Catalogued_Project ("Simple logging to console");

   Repo : constant URL := "https://github.com/alire-project/simple_logging.git";

   V_1_0 : constant Release :=
             Project.Register
               (V ("1.0"),
                Git (Repo, "d98242b8bd1c7f964cebc454e9b1206ffdbb0ca9"));

end Alire.Index.Simple_Logging;
