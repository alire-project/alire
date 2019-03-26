with Alire.Index.GNAT;

package Alire.Index.NcursesAda is

   function Project is
     new Catalogued_Project ("Ada binding to the ncurses text interface library");

   Base : constant Release := Project.Unreleased
     (Dependencies => GNAT.Project.Current);

   package V_6_1_20180127 is new Project_Release
     (Base.Replacing
        (Origin =>
              Native ((Debian |
                       Ubuntu => Packaged_As ("libncursesada6.1.20180127-dev"),
                       others => Unavailable))));

   package V_6 is new Project_Release
     (Base.Replacing
        (Origin =>
              Native ((Debian |
                       Ubuntu => Packaged_As ("libncursesada5-dev"),
                       others => Unavailable))));

   package V_5 is new Project_Release
     (Base.Replacing
        (Origin =>
              Native ((Debian |
                       Ubuntu => Packaged_As ("libncursesada3-dev"),
                       others => Unavailable))));

end Alire.Index.NcursesAda;
