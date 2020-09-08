with Alire.Origins;
with Alire.URI;

package Alire.Publish is

   procedure Local_Repository (Path     : Any_Path := ".";
                               Revision : String   := "") with
     Pre => URI.Scheme (Path) in URI.File_Schemes;
   --  Check that given Path is an up-to-date git repo with URL in the
   --  whitelist. If so, proceed with remote repo verification. If no revision
   --  given use the HEAD commit, otherwise use the revision (tag, branch,
   --  commit) commit.

   procedure Remote_Origin (URL    : Alire.URL;
                            Commit : String := "");
   --  Requires a remote URL to a source file or a git repository. Commit is
   --  mandatory in the latter case. Produces a file `crate-version.toml` in
   --  the current directory or raises Checked_Error with the appropriate error
   --  message set.

   procedure Print_Trusted_Sites;
   --  Print our list of allowed sites to host git releases

end Alire.Publish;
