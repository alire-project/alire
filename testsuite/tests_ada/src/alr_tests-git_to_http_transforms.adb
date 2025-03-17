with Alire.VCSs.Git;

procedure Alr_Tests.Git_To_HTTP_Transforms is

   -----------------------
   -- Check_Git_To_HTTP --
   -----------------------

   procedure Check_Git_To_HTTP is
      use VCSs.Git;
   begin
      --  Proper transform starting without .git
      pragma Assert (Transform_To_Public ("git@github.com:user/project") =
                       "git+https://github.com/user/project");

      --  Proper transform starting with .git
      pragma Assert (Transform_To_Public ("git@github.com:user/project.git") =
                       "git+https://github.com/user/project.git");

      --  GitLab
      pragma Assert (Transform_To_Public ("git@gitlab.com:user/project") =
                       "git+https://gitlab.com/user/project");

      --  Unknown site, not transformed
      pragma Assert (Transform_To_Public ("git@ggithub.com:user/project") =
                       "git@ggithub.com:user/project");

      --  No-op for HTTPS
      pragma Assert (Transform_To_Public ("https://github.com/user/project") =
                       "https://github.com/user/project");
   end Check_Git_To_HTTP;

begin
   Check_Git_To_HTTP;
end Alr_Tests.Git_To_HTTP_Transforms;
