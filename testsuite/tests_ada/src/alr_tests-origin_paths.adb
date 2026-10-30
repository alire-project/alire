pragma Alire_Test;
with AAA.Strings;

with Alire.Origins;
with Alire.VFS;

procedure Alr_Tests.Origin_Paths is

   --  Paths in index origins must stay below the folder they are relative to,
   --  whatever the platform: subdirs are contained paths (portable, and not
   --  going above the origin root), and archive names plain file names.

   procedure Check (Error : String; Path : String; Reason : String) is
   begin
      if Reason = "" then
         pragma Assert (Error = "", "Should be valid: " & Path & ": " & Error);
      else
         pragma Assert (Error /= "", "Should be invalid: " & Path);
         pragma Assert (AAA.Strings.Contains (Error, Reason),
                        "Unexpected reason for " & Path & ": " & Error);
      end if;
   end Check;

   procedure Subdir (Path : String; Reason : String := "") is
   begin
      Check (VFS.Error_In_Contained_Path (Path, What => "subdir"),
             Path, Reason);
   end Subdir;

   procedure Archive (Name : String; Reason : String := "") is
   begin
      Check (Origins.Error_In_Archive_Name (Name), Name, Reason);
   end Archive;

begin
   --  Valid subdirs, as found in the community index

   Subdir ("");
   Subdir ("src");
   Subdir ("src/sub");
   Subdir ("./src/");
   Subdir ("./.alire/src/");
   Subdir ("a..b/..c/d..");
   Subdir ("src/..");
   Subdir ("a/../b");

   --  Invalid subdirs

   Subdir ("..",                 "above the root");
   Subdir ("../src",             "above the root");
   Subdir ("src/../..",          "above the root");
   Subdir ("./src/../../other",  "above the root");
   Subdir ("/src",               "relative");
   Subdir ("//server/share",     "relative");
   Subdir ("C:/src",             "':'");
   Subdir ("C:src",              "':'");
   Subdir ("src\sub",            "forward slashes");
   Subdir ("\\server\share",     "forward slashes");
   Subdir ("..\src",             "forward slashes");

   --  Portable paths, unlike contained ones, may leave their folder, as
   --  pins to sibling folders do

   pragma Assert (VFS.Error_In_Portable_Path ("../sibling") = "");
   pragma Assert (VFS.Is_Portable ("../sibling"));
   pragma Assert (VFS.Is_Portable ("./a/../b/"));
   pragma Assert (not VFS.Is_Portable ("/abs"));
   pragma Assert (not VFS.Is_Portable ("C:/abs"));
   pragma Assert (not VFS.Is_Portable ("C:rel"));
   pragma Assert (not VFS.Is_Portable ("a\b"));

   --  Errors name the field and the path

   Check (VFS.Error_In_Contained_Path ("../x", What => "subdir"),
          "../x", "invalid subdir '../x'");

   --  Valid archive names

   Archive ("crate-1.0.0.tar.gz");
   Archive ("v1.0.0.zip");
   Archive ("crate..1.tgz");

   --  Invalid archive names

   Archive ("",                     "file name");
   Archive (".",                    "file name");
   Archive ("..",                   "file name");
   Archive ("../crate.tar.gz",      "plain file name");
   Archive ("dir/crate.tar.gz",     "plain file name");
   Archive ("/tmp/crate.tar.gz",    "plain file name");
   Archive ("..\crate.tar.gz",      "plain file name");
   Archive ("C:crate.tar.gz",       "plain file name");
   Archive ("C:\tmp\crate.tar.gz",  "plain file name");
end Alr_Tests.Origin_Paths;
