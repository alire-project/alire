--  Deprecated: This package provides a depracated version of license
--  identification and will be removed in a future release.

package Alire.Licensing with Preelaborate is

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
                     Custom,
                     Unknown);

   function License_Labels (L : Licenses)
                            return String
   is
     (case L is
      when AFL_3_0              => "AFL 3.0",
      when AGPL_3_0             => "AGPL 3.0",
      when Apache_2_0           => "Apache 2.0",
      when Artistic_2_0         => "Artistic 2.0",
      when BSD_2_Clause         => "BSD 2-Clauses",
      when BSD_3_Clause_Clear   => "BSD 3-Clauses Clear",
      when BSD_3_Clause         => "BSD 3-Clauses",
      when BSL_1_0              => "BSL 1.0",
      when CC0_1_0              => "CC0 1.0",
      when CC_BY_4_0            => "CC BY 4.0",
      when CC_BY_SA_4_0         => "CC BY-SA 4.0",
      when ECL_2_0              => "ECL 2.0",
      when EPL_1_0              => "EPL 1.0",
      when EPL_2_0              => "EPL 2.0",
      when EUPL_1_1             => "EUPL 1.1",
      when EUPL_1_2             => "EUPL 1.2",
      when GPL_2_0              => "GPL 2.0",
      when GPL_3_0              => "GPL 3.0",
      when ISC                  => "ISC",
      when LGPL_2_1             => "LGPL 2.1",
      when LGPL_3_0             => "LGPL 3.0",
      when LPPL_1_3c            => "LPPL 1.3c",
      when MIT                  => "MIT",
      when MPL_2_0              => "MPL 2.0",
      when MS_PL                => "MS PL",
      when MS_RL                => "MS RL",
      when NCSA                 => "NCSA",
      when OFL_1_1              => "OFL 1.1",
      when OSL_3_0              => "OSL 3.0",
      when PostgreSQL           => "PostgreSQL",
      when Unlicense            => "Unlicense",
      when WTFPL                => "WTFPL",
      when Zlib                 => "zlib",

      --  Extra additions
      when GMGPL_2_0            => "GMGPL 2.0",
      when GMGPL_3_0            => "GMGPL 3.0",
      when Public_Domain        => "Public Domain",
      when Custom               => "Custom",
      when Unknown              => "Unknown");

   function From_String (Label : String) return Licenses;
   --  Return the Licenses value corresponding to Label (see License_Labels).
   --  Return Unknown if none matches.

end Alire.Licensing;
