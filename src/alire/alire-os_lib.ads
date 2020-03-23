package Alire.OS_Lib with Preelaborate is

   function "/" (L, R : String) return String;
   --  Shorthand for path composition

   --  Package to enable easy use of "/"
   package Operators is
      function "/" (L, R : String) return String renames OS_Lib."/";
   end Operators;

   procedure Bailout (Code : Integer := 0);

   function Exe_Suffix return String;

   function Getenv (Name : String; Default : String := "") return String;

   procedure Setenv (Name : String; Value : String);

end Alire.OS_Lib;
