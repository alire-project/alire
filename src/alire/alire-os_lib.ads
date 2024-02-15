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

   function Locate_Exec_On_Path (Exec_Name : String) return String;
   --  Return the location of an executable if found on PATH, or "" otherwise.
   --  On Windows, no need to append ".exe" as it will be found without it.

end Alire.OS_Lib;
