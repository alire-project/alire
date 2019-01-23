with Ada.Strings.Unbounded;

package Alire.Licensing is

   --  From https://github.com/github/choosealicense.com

   type Licenses is (AFL_3_0,
                     AGPL_3_0,
                     Apache_2_0,
                     Artistic_2_0,
                     BSD_2_Clause,
                     BSD_3_Clause_Clear,
                     BSD_3_Clause,
                     BSL_1_0,
                     CC0_1_0,
                     CC_BY_4_0,
                     CC_BY_SA_4_0,
                     ECL_2_0,
                     EPL_1_0,
                     EPL_2_0,
                     EUPL_1_1,
                     EUPL_1_2,
                     GPL_2_0,
                     GPL_3_0,
                     ISC,
                     LGPL_2_1,
                     LGPL_3_0,
                     LPPL_1_3c,
                     MIT,
                     MPL_2_0,
                     MS_PL,
                     MS_RL,
                     NCSA,
                     OFL_1_1,
                     OSL_3_0,
                     PostgreSQL,
                     Unlicense,
                     WTFPL,
                     Zlib,

                     --  Extra additions
                     GMGPL_2_0,
                     GMGPL_3_0,
                     Public_Domain,
                     Unknown);

   License_Labels : constant array (Licenses)
                             of Ada.Strings.Unbounded.Unbounded_String :=
     (AFL_3_0              => +"AFL 3.0",
      AGPL_3_0             => +"AGPL 3.0",
      Apache_2_0           => +"Apache 2.0",
      Artistic_2_0         => +"Artistic 2.0",
      BSD_2_Clause         => +"BSD 2-Clauses",
      BSD_3_Clause_Clear   => +"BSD 3-Clauses Clear",
      BSD_3_Clause         => +"BSD 3-Clauses",
      BSL_1_0              => +"BSL 1.0",
      CC0_1_0              => +"CC0 1.0",
      CC_BY_4_0            => +"CC BY 4.0",
      CC_BY_SA_4_0         => +"CC BY-SA 4.0",
      ECL_2_0              => +"ECL 2.0",
      EPL_1_0              => +"EPL 1.0",
      EPL_2_0              => +"EPL 2.0",
      EUPL_1_1             => +"EUPL 1.1",
      EUPL_1_2             => +"EUPL 1.2",
      GPL_2_0              => +"GPL 2.0",
      GPL_3_0              => +"GPL 3.0",
      ISC                  => +"ISC",
      LGPL_2_1             => +"LGPL 2.1",
      LGPL_3_0             => +"LGPL 3.0",
      LPPL_1_3c            => +"LPPL 1.3c",
      MIT                  => +"MIT",
      MPL_2_0              => +"MPL 2.0",
      MS_PL                => +"MS PL",
      MS_RL                => +"MS RL",
      NCSA                 => +"NCSA",
      OFL_1_1              => +"OFL 1.1",
      OSL_3_0              => +"OSL 3.0",
      PostgreSQL           => +"PostgreSQL",
      Unlicense            => +"Unlicense",
      WTFPL                => +"WTFPL",
      Zlib                 => +"zlib",

      --  Extra additions
      GMGPL_2_0            => +"GMGPL 2.0",
      GMGPL_3_0            => +"GMGPL 3.0",
      Public_Domain        => +"Public Domain",
      Unknown              => +"Unknown");

   function From_String (Label : String) return Licenses;
   --  Return the Licenses value corresponding to Label (see License_Labels).
   --  Return Unknown if none matches.

end Alire.Licensing;
