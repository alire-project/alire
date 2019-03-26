package Alire.Index.Ini_Files is

   function Project is new Catalogued_Project
     ("A standalone, portable Ada package for configuration files");

   Base : constant Release := Project.Unreleased
     (Version    => V ("08"),
      Properties => Project_File ("ini_files_gnat.gpr")
                    and Author  ("Gautier de Montmollin & Rolf Ebert")
                    and Website ("https://sourceforge.net/p/ini-files/")
                    and License (MIT));

   package V_8 is new Project_Release
     (Base
      .Renaming (Project)
      .Replacing (SVN ("https://svn.code.sf.net/p/ini-files/code/", "28")));

end Alire.Index.Ini_Files;
