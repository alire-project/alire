package Alire.Index.Smart_Pointers is

   function Project is new Catalogued_Project
     ("Christoph Karl Walter Grein's Smart Pointers ");

   Repo : constant URL := "https://github.com/alire-project/smart_pointers.git";

   Base : constant Release :=
            Project.Unreleased
              (Properties =>
                 Author     ("Christoph Karl Walter Grein") and
                 Website    ("http://www.christ-usch-grein.homepage.t-online.de/Ada/Smart_Pointers.html") and
                 License    (Unknown));

   package V_20180216 is new Project_Release
     (Base.Replacing (Git (Repo, "01f2674634dc23da1a572363d8660af274642771")));

end Alire.Index.Smart_Pointers;
