pragma Alire_Test;
with AAA.Strings;

with Alire.Index;

procedure Alr_Tests.Release_Versions is

   --  Release versions in index manifests. Semantic versioning rules are
   --  checked by the Semantic_Versioning library (and tested there); here we
   --  check that they are applied, and alire's own limit on the length after
   --  X.Y.Z.

   use Alire.Index;

   function Chars (Length : Natural) return String
   is (1 .. Length => 'a');
   --  Filler for pre-release and build metadata of a given length

   Max : constant Natural := Max_Version_Suffix_Length;
   --  Characters allowed after X.Y.Z, separators included

   procedure Valid (Version : String) is
      Error : constant String := Error_In_Release_Version (Version);
   begin
      pragma Assert (Error = "", "Should be valid: " & Version & ": " & Error);
   end Valid;

   procedure Invalid (Version : String; Reason : String) is
      Error : constant String := Error_In_Release_Version (Version);
   begin
      pragma Assert (Error /= "", "Should be invalid: " & Version);
      pragma Assert (AAA.Strings.Contains (Error, Reason),
                     "Unexpected reason for " & Version & ": " & Error);
   end Invalid;

begin
   --  Valid versions

   Valid ("1.0.0");
   Valid ("2025.4.18");
   Valid ("1.0.0-rc.1+20220920");

   --  Invalid versions, as reported by the library

   Invalid ("1.0",        "three numbers");
   Invalid ("1.0.0 x",    "spaces");
   Invalid ("2025.04.18", "leading zeros");
   Invalid ("1.0.0-x_y",  "Invalid character");
   Invalid ("1.0.0+",     "Empty build metadata");
   Invalid ("1000000000000000000.0.0", "too large");

   --  Length after X.Y.Z, separators included

   Valid ("1.0.0-" & Chars (Max - 1));
   Valid ("1.0.0+" & Chars (Max - 1));
   Valid ("1.0.0-" & Chars (10) & "+" & Chars (Max - 12));

   Invalid ("1.0.0-" & Chars (Max), "longer than");
   Invalid ("1.0.0+" & Chars (Max), "longer than");
   Invalid ("1.0.0-" & Chars (10) & "+" & Chars (Max - 11), "longer than");

   --  X.Y.Z is not limited by the suffix length

   Valid ("999999999999999999.999999999999999999.999999999999999999-"
          & Chars (Max - 1));
end Alr_Tests.Release_Versions;
