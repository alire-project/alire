package Alire.Index.RSFile is

   function Project is new Catalogued_Project
     ("Pick a file with probability proportional to its size");

   Repo : constant URL := "https://github.com/mosteo/rsfile.git";

   Base : constant Release :=
            Project.Unreleased
              (Properties =>
                 Author     ("alejandro@mosteo.com") and
                 License    (GPL_3_0));

   package V_1_0_0 is new Project_Release
     (Base
      .Replacing (Git (Repo, "d1a368be007c9e4dd92cdaa9d031324121b9f4c7"))
      .Extending (Properties => Executable ("rsfile")));

end Alire.Index.RSFile;
