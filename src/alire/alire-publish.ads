with Alire.Roots;
with Alire.URI;

package Alire.Publish is

   type All_Options is private;

   function New_Options (Skip_Build : Boolean := False;
                         Manifest   : String  := Roots.Crate_File_Name)
                         return All_Options;

   procedure Directory_Tar (Path     : Any_Path := ".";
                            Revision : String   := "HEAD";
                            Options  : All_Options := New_Options);
   --  Publish the release at the given directory, by creating a source archive
   --  to be uploaded somewhere. Then proceed with Remote_Origin using the
   --  uploaded archive. If a git repo is at Path, `git archive` will be
   --  used; otherwise `tar` will be used.

   procedure Local_Repository (Path     : Any_Path := ".";
                               Revision : String   := "HEAD";
                               Options  : All_Options := New_Options) with
     Pre => URI.Scheme (Path) in URI.File_Schemes;
   --  Check that given Path is an up-to-date git repo. If so, proceed with
   --  remote repo verification. If no revision given use the HEAD commit,
   --  otherwise use the revision (tag, branch, commit) commit.

   procedure Remote_Origin (URL     : Alire.URL;
                            Commit  : String := "";
                            Subdir  : Relative_Path := "";
                            Options : All_Options := New_Options);
   --  Requires a remote URL to a source file or a git repository. Commit is
   --  mandatory in the latter case. If Subdir is /= "", it is a relative path
   --  inside a repository with the actual location of a nested crate. Produces
   --  a file `crate-version.toml` in the current directory or raises
   --  Checked_Error with the appropriate error message set.

   procedure Print_Trusted_Sites;
   --  Print our list of allowed sites to host git releases

private

   type All_Options is tagged record
      Manifest_File : UString;
      Skip_Build    : Boolean := False;
   end record;

   function Manifest (Options : All_Options) return Any_Path
   is (+Options.Manifest_File);

   function Nonstandard_Manifest (Options : All_Options) return Boolean
   is (Options.Manifest /= Roots.Crate_File_Name);

end Alire.Publish;
