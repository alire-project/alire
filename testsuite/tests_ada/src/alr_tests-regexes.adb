with Alire.Utils.Regex;

procedure Alr_Tests.Regexes is
   use Alire.Utils.Regex;

   GH_Token_Regex : constant String := "(github_pat|gh[pours])_[a-f0-9]+";

begin
   --  Partial matches with unanchored expressions
   pragma Assert (Matches ("abc", "abcde"), "Failed unanchored match at beginning");
   pragma Assert (Matches ("cde", "abcde"), "Failed unanchored match at end");
   pragma Assert (Matches ("bcd", "abcde"), "Failed unanchored match in the middle");

   --  Full matches with unanchored expressions
   pragma Assert (Fully_Matches ("abc", "abc"), "Failed full match");
   pragma Assert (not Fully_Matches ("abc", "abcde"), "Should not partially match");

   --  Anchored full matches
   pragma Assert (Matches ("^abc$", "abc"), "Failed full anchored match");
   pragma Assert (Fully_Matches ("^abc$", "abc"), "Failed redundant full anchored match");

   --  Some failing matches
   pragma Assert (not Matches ("abc", "def"), "Should not match different strings");
   pragma Assert (not Matches ("abc", "ab"), "Should not match shorter string");
   pragma Assert (not Fully_Matches ("abc", "abcd"), "Should not match longer string");

   --  Some actual regex tests
   pragma Assert (Matches ("^\d{3}-\d{2}-\d{4}$", "123-45-6789"), "Failed regex match for SSN format");
   pragma Assert (Fully_Matches ("[0-9a-f]{8}", "1a2b3c4d"), "Failed regex match for hex format");
   pragma Assert (not Fully_Matches ("[0-9a-f]{8}", "1a2b3c4d5"), "Should not match longer hex format");
   pragma Assert (Fully_Matches (GH_Token_Regex, "github_pat_1234567890abcdef"),
                  "Failed regex match for GitHub fine-grained PAT format");
   pragma Assert (Fully_Matches (GH_Token_Regex, "ghp_1234567890abcdef"),
                  "Failed match for new GitHub PAT format");
   pragma Assert (not Fully_Matches (GH_Token_Regex, "MY_PERSONAL_TOKEN"),
                  "Should not match invalid GitHub PAT format");
end Alr_Tests.Regexes;