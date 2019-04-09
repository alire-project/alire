package Alire.Index.ZLib is

   function Project is new Catalogued_Project
     ("Library implementing the deflate method from gzip/PKZIP");

   Base : constant Release := Project.Unreleased;

   package V_1_2 is new Project_Release
     (Base
      .Replacing
        (Native ((Debian | Ubuntu => Packaged_As ("zlib1g-dev"),
                  others          => Unavailable))));

end Alire.Index.ZLib;
