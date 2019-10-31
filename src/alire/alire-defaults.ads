package Alire.Defaults with Preelaborate is

   Community_Index : constant URL :=
                       "git+https://github.com/alire-project/alire-index";
   --  Default index installed on first run

   Community_Index_Name : constant Restricted_Name := "community";

   Description : constant String := "Shiny new project";
   --  TODO: replace this constant with a function that returns random fortunes

   Maintainer  : constant String := "your@email.here";

   Maintainer_Login : constant String := "github-username";
   --  A well-formed GitHub username

end Alire.Defaults;
