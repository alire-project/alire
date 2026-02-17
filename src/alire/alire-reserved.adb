package body Alire.Reserved is

   type String_Access is access constant String;
   type String_Array is array (Positive range <>) of String_Access;

   --  list of Ada2022 and GPR keywords
   Reserved_Keywords : access String_Array;

   ----------------
   -- Is_Keyword --
   ----------------

   function Is_Keyword (S : String) return Boolean
   is
   begin
      if Reserved_Keywords = null then
         Reserved_Keywords :=
           new String_Array'
             (
              new String'("abort"),
              new String'("abs"),
              new String'("abstract"),
              new String'("accept"),
              new String'("access"),
              new String'("aliased"),
              new String'("all"),
              new String'("and"),
              new String'("array"),
              new String'("at"),
              new String'("begin"),
              new String'("body"),
              new String'("case"),
              new String'("constant"),
              new String'("declare"),
              new String'("delay"),
              new String'("delta"),
              new String'("digits"),
              new String'("do"),
              new String'("else"),
              new String'("elsif"),
              new String'("end"),
              new String'("entry"),
              new String'("exception"),
              new String'("exit"),
              new String'("extends"),
              new String'("external"),
              new String'("external_as_list"),
              new String'("for"),
              new String'("function"),
              new String'("generic"),
              new String'("goto"),
              new String'("if"),
              new String'("in"),
              new String'("interface"),
              new String'("is"),
              new String'("limited"),
              new String'("loop"),
              new String'("mod"),
              new String'("new"),
              new String'("not"),
              new String'("null"),
              new String'("of"),
              new String'("or"),
              new String'("others"),
              new String'("out"),
              new String'("overriding"),
              new String'("package"),
              new String'("parallel"),
              new String'("pragma"),
              new String'("private"),
              new String'("procedure"),
              new String'("project"),
              new String'("protected"),
              new String'("raise"),
              new String'("range"),
              new String'("record"),
              new String'("rem"),
              new String'("renames"),
              new String'("requeue"),
              new String'("return"),
              new String'("reverse"),
              new String'("select"),
              new String'("separate"),
              new String'("some"),
              new String'("subtype"),
              new String'("synchronized"),
              new String'("tagged"),
              new String'("task"),
              new String'("terminate"),
              new String'("then"),
              new String'("type"),
              new String'("until"),
              new String'("use"),
              new String'("when"),
              new String'("while"),
              new String'("with"),
              new String'("xor")
             );
      end if;

      return (for some K of Reserved_Keywords.all => S = K.all);
   end Is_Keyword;

end Alire.Reserved;
