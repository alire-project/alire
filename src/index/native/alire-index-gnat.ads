package Alire.Index.GNAT is

   function Project is new Catalogued_Project
     ("GNAT is a compiler for the Ada programming language");

   Base : constant Release := Project.Unreleased
     (Properties =>
        Path ("/usr/bin"),

      Available_When =>
        Target = Native);

   --  If minor versions proved important they could be segregated with platform-specific knowledge

   package V_7 is new Project_Release
     (Base.Replacing
        (Origin => Native ((Debian | Ubuntu => Packaged_As ("gnat-7"),
                            others          => Unavailable))));

end Alire.Index.GNAT;
