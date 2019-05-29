with Alire.Index;
with Alire.Properties;
with Alire.Releases;

with Alr.Paths;
with Alr.Query;

package Alr.Checkout is

   procedure Working_Copy (R               : Alire.Index.Release;
                           Parent_Folder   : String;
                           Generate_Files  : Boolean := True;
                           Perform_Actions : Boolean := True);
   --  A working copy might not have alr and gpr files, that will be generated
   --  if needed.

   procedure To_Folder (Projects : Query.Instance;
                        Parent   : String := Paths.Projects_Folder);
   --  Retrieves all releases into a folder, typically the main cache.
   --  One project in the solution (typically the root project itself) can be
   --  ignored.

end Alr.Checkout;
