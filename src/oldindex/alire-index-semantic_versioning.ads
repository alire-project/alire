package Alire.Index.Semantic_Versioning is

   function Project is
     new Catalogued_Project ("Semantic Versioning in Ada");

   Repo : constant URL := "https://github.com/alire-project/semantic_versioning.git";

   Base : constant Release := Project.Unreleased;

   package V_0_3_2 is new Project_Release
     (Base.Replacing (Git (Repo, "dc462f11adb34bbb3d9163e44c287add5b3421c6")));

   --  Old style releases

   V_0_3_1 : constant Release := Project.Register
     (V ("0.3.1"),
      Git (Repo, "71653babaab97d835ecd22edb562b9b529dd8948"));

   V_0_3 : constant Release := Project.Register
     (V ("0.3"),
      Git (Repo, "ebf71f00daba91489238f79819078a37e36be188"));


   V_0_2 : constant Release := Project.Register
     (V ("0.2"),
      Git (Repo, "2937c650511ad3c87af64be963eca7eba7aebb68"));

   V_0_1_2 : constant Release := Project.Register
     (V ("0.1.2"),
      Git (Repo, "09774d80fac62ea3a09d46b22d4807da530387e2"));

end Alire.Index.Semantic_Versioning;
