with Alire.Origins;

package Alire.Publish is

   procedure Verify_And_Create_Index_Manifest (Origin : URL;
                                               Commit : String := "");
   --  Requires a remote URL to a source file or a git repository. Commit is
   --  mandatory in the latter case. Produces a file `crate-version.toml` in
   --  the current directory or raises Checked_Error with the appropriate error
   --  message set.

end Alire.Publish;
