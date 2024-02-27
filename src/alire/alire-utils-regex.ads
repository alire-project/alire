package Alire.Utils.Regex is

   function Escape (Literal : String) return String;
   --  Prepare a string to be interpreted literally by GNAT.Regpat, like e.g.
   --  "stdc++" --> "stdc\+\+"

   function First_Match (Regex : String; Text : String) return String
     with Pre => (for some Char of Regex => Char = '(');
   --  Wrapper on GNAT.Regpat. It returns the first match found, which is not
   --  necessarily the first parenthesized expression. E.g., in a pattern like:
   --  (abc)|(efg), it will return the "efg" match, even if to GNAT.Regpat that
   --  is the second matching expression. In case of no match, it will return
   --  an empty string. At least one capture must be attempted in the Regex.

end Alire.Utils.Regex;
