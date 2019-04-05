with Alire.Index.NcursesAda;

package Alire.Index.Adacurses is

   function Project is
     new Catalogued_Project ("Wrapper on different packagings of NcursesAda");

   Repo : constant String := "https://github.com/alire-project/adacurses-wrapper.git";
   --  This is the wrapper around native packages

   Repo_Src : constant String := "https://github.com/alire-project/adacurses.git";
   --  This is the actual source

   Comments : constant Conditional.Properties :=
                Comment ("AdaCurses is the project name used by upstream, thus adacurses.gpr") and
                Comment ("However, some distros (e.g., Debian family) use ncursesada.gpr") and
                Comment ("This package wraps these differences so clients can always safely use adacurses");

   Base : constant Release := Project.Unreleased
     (Properties =>
        Comments and
        Author ("Thomas E. Dickey") and
        Website ("http://invisible-island.net/ncurses/ncurses-Ada95.html"));

--     package V_6_1 is new Project_Release
--       (Base
--        .Replacing
--          (Git (Repo_Src, "fa61672dbb457fcd1dcc38da6f1d0681aaf0cd39"))
--        .Extending
--          (Private_Properties =>
--              Action_Run (Post_Fetch, "./configure") and
--              Action_Run (Post_Fetch, "make"),
--
--           Available          =>
--              Operating_System = GNU_Linux));

   package V_6 is new Project_Release
     (Base
      .Replacing
        (Git (Repo, "4ccb20409becb50c0b5fd29effb676b650608326"))
      .Extending
        (Case_Distribution_Is
             ((Debian | Ubuntu => NcursesAda.V_6.Within_Major,
               others          => Unavailable))));

   package V_5 is new Project_Release
     (Base
      .Replacing
        (Git (Repo, "4ccb20409becb50c0b5fd29effb676b650608326"))
      .Extending
        (Case_Distribution_Is
             ((Debian | Ubuntu => NcursesAda.V_5.Within_Major,
               others          => Unavailable))));

end Alire.Index.Adacurses;
