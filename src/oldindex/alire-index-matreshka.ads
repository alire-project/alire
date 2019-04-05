package Alire.Index.Matreshka is

   --  The build process of this package is a bit special.
   --  As other big projects, it includes a configuration step.
   --  In this case is an Ada tool that we can build in advance and use.

   --  There is no easy way in current Alire to individually select components
   --  to build, which furthermore have differing dependencies. For now,
   --  only components without dependencies are built then.

--     package No_DB is
--
--        function Project is new Catalogued_Project
--          ("Ada framework to develop information systems (databases missing)");
--
--        Repo : constant URL := "https://github.com/reznikmm/matreshka.git";
--
--        Install_To : constant String := "alrinst";
--
--        Base       : constant Release := Project.Unreleased
--          (Properties         =>
--             Project_File (Install_To & "/lib/gnat/matreshka/league.gpr") and
--
--             Maintainer ("Vadim Godunko") and
--             Maintainer ("Maxim Reznik") and
--             Website    ("http://forge.ada-ru.org/matreshka") and
--             License    (BSD_3_Clause) and
--             Comment    ("NOTE: ONLY COMPONENTS WITHOUT DEPENDENCIES IN THIS PACKAGE"),
--
--           Private_Properties =>
--             Action_Run (Post_Fetch, "make config") and
--             Action_Run (Post_Fetch,
--               "./configure --prefix=" & Install_To &
--                 " --enable-amf" &
--                 " --disable-sqlite3 --disable-postgresql --disable-firebird --disable-oracle --disable-mysql") and
--             Action_Run (Post_Fetch, "make") and
--             Action_Run (Post_Fetch, "make install"),
--
--           Available_When     =>
--             Operating_System /= Windows);
--
--        package V_0_7 is new Project_Release
--          (Base
--           .Replacing
--             (Git (Repo, "9ce672ea383179392bdad3967fa37537db72fa20")));
--
--     end No_DB;


end Alire.Index.Matreshka;
