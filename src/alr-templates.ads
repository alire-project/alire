with Alire.Releases;

with Alr.Query;

package Alr.Templates is

   procedure Generate_Full_Index (Session_Path, Index_Folder : String);
   --  Generates alr-index.ads that with-es all available alire-index-*.ads releases

   procedure Generate_Agg_Gpr (Instance : Query.Instance;
                               Root     : Alire.Releases.Release);
   --  Generate the aggregate project file for path setup

   procedure Generate_Prj_Alr (Instance : Query.Instance;
                               Root     : Alire.Releases.Release;
                               Exact    : Boolean := True;
                               Filename : String  := "");
   --  Generate the dependencies file (Name_Alr.ads)
   --  If root /= "" then its dependency is skipped (to not depend on itself)
   --  File can be a full path + filename, otherwise Current_Folder / Alr_Index_File is used
   --  If exact use "Exactly" dependencies, otherwise use "At_Least_Within_Major"

   procedure Generate_Session (Session_Path : String;
                               Alire_File   : String := "");
   --  Generate the alr-session.ads file for current project file
   --  If no alr file, an empty hash will be used so this will be an outdated or stand-alone executable
   --  This is needed when, for example, searching or getting projects: we are outside any project but
   --    need the full index.

   Sed_Pattern : constant String;

private

   Sed_Pattern : constant String := "PROJECT_SKEL";
   --  In Caps so its proper use in lowercase or Mixed Case is ensured

end Alr.Templates;
