package Alire.Index.Libgsl is

   function Project is new Catalogued_Project
     ("The GNU Scientific Library (GSL)");

   Base : constant Release := Project.Unreleased;

   package V_0 is new Project_Release
     (Base
      .Replacing
        (Native ((Debian | Ubuntu => Packaged_As ("libgsl-dev"),
                  others          => Unavailable))));

end Alire.Index.Libgsl;
