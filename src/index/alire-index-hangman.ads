package Alire.Index.Hangman is

   function Project is
     new Catalogued_Project ("Hangman game for the console");

   Prj_Repo       : constant URL    := "https://github.com/alire-project/Hangman.git";
   Prj_Author     : constant String := "Jon Hollan, Mark Hoffman, & Brandon Ball";

   V_1_0 : constant Release :=
             Project.Register
               (V ("1.0"),
                Git (Prj_Repo, "a57904920b81d72621aba434bf72e0175a12624d"),

                Properties         =>
                  Executable ("hangmain") and
                  Author     (Prj_Author)
               );
end Alire.Index.Hangman;
