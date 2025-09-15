with Alire.GitHub;

procedure Alr_Tests.Github_Tokens is
   use Alire.GitHub;
begin
   pragma Assert (Is_Possibly_A_Token ("12345678abcdefab12345678abcdefab12345678"),
                  "Failed for old GitHub PAT format");
   pragma Assert (Is_Possibly_A_Token ("github_pat_1234567890abcdef"),
                  "Failed for GitHub fine-grained PAT format");
   pragma Assert (Is_Possibly_A_Token ("ghp_1234567890abcdef"),
                  "Failed new GitHub PAT format");
   pragma Assert (not Is_Possibly_A_Token ("MY_PERSONAL_TOKEN"),
                  "Should not accept invalid GitHub PAT format");
end Alr_Tests.Github_Tokens;