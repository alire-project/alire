with Alire.Index;
with Alire.Releases;

package Alr.Templates is

   procedure Generate_Project_Alire (Instance : Alire.Index.Instance;
                                     Root     : Alire.Releases.Release);
   --  Generate the dependencies file
   --  If root /= "" then its dependency is skipped (to not depend on itself)

end Alr.Templates;
