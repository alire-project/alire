with Alire.Index;
with Alire.Releases;

package Alr.Templates is

   procedure Generate_Index (Session_Path, Index_Folder : String);
   --  Generates alr-index.ads that with-es all available alire-index-*.ads releases

   procedure Generate_Gpr (Instance : Alire.Index.Instance;
                           Root     : Alire.Releases.Release);
   --  Generate the aggregate project file for path setup

   procedure Generate_Project_Alire (Instance : Alire.Index.Instance;
                                     Root     : Alire.Releases.Release);
   --  Generate the dependencies file
   --  If root /= "" then its dependency is skipped (to not depend on itself)

   procedure Generate_Session (Session_Path, Alire_File : String);
   --  Generate the alr-session.ads file for current project file

end Alr.Templates;
