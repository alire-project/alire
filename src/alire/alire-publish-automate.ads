with Alire.Root;

package Alire.Publish.Automate is

   --  Facilities to automatically submit PRs to Github

   procedure Create_PR
     with Pre => Root.Current.Is_Valid;

end Alire.Publish.Automate;
