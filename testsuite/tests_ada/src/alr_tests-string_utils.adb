with Alire.Utils; use Alire.Utils;

procedure Alr_Tests.String_Utils is
begin
   pragma Assert (Strip_Prefix ("aaabbb", "aaa") = "bbb");
   pragma Assert (Strip_Prefix ("aaabbb", "aa") = "abbb");
   pragma Assert (Strip_Prefix ("aabbb", "aaa") = "aabbb");

   pragma Assert (Strip_Suffix ("aaabbb", "bbb") = "aaa");
   pragma Assert (Strip_Suffix ("aaabbb", "bb") = "aaab");
   pragma Assert (Strip_Suffix ("aaabb", "bbb") = "aaabb");

   pragma Assert (Left_Pad ("xxx", 5) = "  xxx");
   pragma Assert (Left_Pad ("xxxxx", 3) = "xxxxx");
   pragma Assert (Left_Pad ("x", 4, 'y') = "yyyx");

   pragma Assert (Right_Pad ("xxx", 5) = "xxx  ");
   pragma Assert (Right_Pad ("xxxxx", 3) = "xxxxx");
   pragma Assert (Right_Pad ("x", 4, 'y') = "xyyy");
end Alr_Tests.String_Utils;
