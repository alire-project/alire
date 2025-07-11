pragma Style_Checks (Off);
--  awsres doesn't use proper casing, and fighting GNAT studio is tiresome

with R.Crate_Bin_Alire_Toml;
with R.Crate_Bin_Name_Gpr;
with R.Crate_Bin_Src_Name_Adb;

with R.Crate_Common_Gitignore_Hidden;

with R.Crate_Lib_Alire_Toml;
with R.Crate_Lib_Name_Gpr;
with R.Crate_Lib_Src_Name_Ads;

with R.Crate_Test_Tests_Alire_Toml;
with R.Crate_Test_Tests_Common_Name_Tests_Ads;
with R.Crate_Test_Tests_Crate_Test_Tests_Gpr;
with R.Crate_Test_Tests_Src_Name_Testsxassertions_Enabled_Adb;

with R.Github_Publish_Yml;
with R.Github_Readme_Md;
with R.Github_Selftest_Yml;

private with Alire.TOML_Adapters;

package Alire.Templates.Builtins is

   --  Hardcoded initializations in Alire

   --  Info needed to initialize a crate, MUST NOT be TOML-escaped (done here),
   --  and its corresponding template name.
   type Crate_Init_Info is record
      Name         : UString;            -- NAME
      Is_Library   : Boolean;            -- IS_LIBRARY
      GitHub_Login : UString;            -- LOGIN
      Username     : UString;            -- USERNAME
      Email        : UString;            -- EMAIL
      Licenses     : UString;            -- LICENSES
      Description  : UString;            -- DESCRIPTION
      Website      : UString;            -- WEBSITE
      Tags         : AAA.Strings.Vector; -- TAGS (expanded: "tag1", "tag2"...)
      With_Test    : Boolean;            -- WITH_TEST
   end record;

   function Init_Crate_Translation (Info : Crate_Init_Info)
                                    return Translations;
   --  Use this translation to initialize crates (trees immediately following)

   --  Files common to binary and library crates
   Crate_Common : constant Tree := New_Tree
     .Append (".gitignore",       +R.Crate_Common_Gitignore_Hidden.Content)
     .Append ("share/@_NAME_@",   New_Dir)
     ;

   --  Files to initialize a binary crate
   Crate_Bin : constant Tree := Crate_Common
     .Append ("alire.toml",       +R.Crate_Bin_Alire_Toml.Content)
     .Append ("@_NAME_@.gpr",     +R.Crate_Bin_Name_Gpr.Content)
     .Append ("src/@_NAME_@.adb", +R.Crate_Bin_Src_Name_Adb.Content)
     ;

   --  Files to initialize a library crate
   Crate_Lib : constant Tree := Crate_Common
     .Append ("alire.toml",       +R.Crate_Lib_Alire_Toml.Content)
     .Append ("@_NAME_@.gpr",     +R.Crate_Lib_Name_Gpr.Content)
     .Append ("src/@_NAME_@.ads", +R.Crate_Lib_Src_Name_Ads.Content)
     ;

   --  Files to initalize a no-skel binary crate
   Crate_Bin_No_Skel : constant Tree := New_Tree
     .Append ("alire.toml",       +R.Crate_Bin_Alire_Toml.Content)
     ;

   --  Files to initialize a no-skel library crate
   Crate_Lib_No_Skel : constant Tree := New_Tree
     .Append ("alire.toml",       +R.Crate_Lib_Alire_Toml.Content)
     ;

   --  Files to initialize a test crate within a regular crate
   Crate_Test : constant Tree := New_Tree
     .Append (".gitignore",                                +R.Crate_Common_Gitignore_Hidden.Content)
     .Append ("alire.toml",                                +R.Crate_Test_Tests_Alire_Toml.Content)
     .Append ("@_NAME_@_tests.gpr",                        +R.Crate_Test_Tests_Crate_Test_Tests_Gpr.Content)
     .Append ("common/@_NAME_@_tests.ads",                 +R.Crate_Test_Tests_Common_Name_Tests_Ads.Content)
     .Append ("src/@_NAME_@_tests-assertions_enabled.adb", +R.Crate_Test_Tests_Src_Name_Testsxassertions_Enabled_Adb.Content)
   ;

   Github : constant Tree := New_Tree
     .Append (".github/workflows/publish.yml",  +R.Github_Publish_Yml.Content)
     .Append (".github/workflows/selftest.yml", +R.Github_Selftest_Yml.Content)
     .Append ("README.md",                      +R.Github_Readme_Md.Content)
   ;

private

   pragma Style_Checks (On);

   use TOML_Adapters;

   ----------------------------
   -- Init_Crate_Translation --
   ----------------------------

   function Init_Crate_Translation (Info : Crate_Init_Info)
                                    return Translations
   is (New_Translation
       .Append ("NAME",        Info.Name)
       .Append ("IS_LIBRARY",  Info.Is_Library)
       .Append ("LOGIN",       Info.GitHub_Login)
       .Append ("USERNAME",    Escape (+Info.Username))
       .Append ("EMAIL",       Info.Email)
       .Append ("LICENSES",    Info.Licenses)
       .Append ("DESCRIPTION", Escape (+Info.Description))
       .Append ("WEBSITE",     Info.Website)
       .Append ("TAGS",
         (if Info.Tags.Is_Empty
          then ""
          else '"' & Info.Tags.Flatten ('"' & ", " & '"') & '"'))
       .Append ("WITH_TEST", Info.With_Test)
      );

end Alire.Templates.Builtins;
