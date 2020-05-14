package Alire.Defaults with Preelaborate is

   Description : constant String := "Shiny new project";
   --  TODO: replace this constant with a function that returns random fortunes

   Docker_Test_Image : constant String := "alire/gnat:debian-stable";
   --  Docker image to be used with `alr test --docker` unless overridden

   Maintainer  : constant String := "your@email.here";

   Maintainer_Login : constant String := "github-username";
   --  A well-formed GitHub username

end Alire.Defaults;
