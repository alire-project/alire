with Ada.Text_IO;

with Alire.Roots;

with Alr.Query;

with Semantic_Versioning;

package Alr.Templates is

   type Generation_Scenarios is
     (Released,  -- Project is already released and packaged
      Unreleased -- Project is a working copy, pinned or some other variation
                 -- with explicit dependencies
     );
   --  In initial generation we know the only dependency is on Alire itself
   --  When pinning we are fixing to current resolved versions
   --  Otherwise who knows

   procedure Generate_Agg_Gpr (Instance : Query.Instance;
                               Root     : Alire.Roots.Root);
   --  Generate the aggregate project file with given resolved dependencies

   procedure Generate_Agg_Gpr (Root : Alire.Roots.Root);
   --  Generate the aggregate project file solving the dependencies of the given root

   procedure Generate_Prj_Alr (Scenario : Generation_Scenarios;
                               Project  : Alire.Project;
                               Version  : Semantic_Versioning.Version :=
                                 Semantic_Versioning.V ("0");
                               Deps     : Types.Platform_Dependencies :=
                                 Types.No_Dependencies);
   --  Generate dependency file, either for a released or unreleased project

   procedure Generate_Session (Session_Path : String;
                               Full_Index   : Boolean;
                               Alire_File   : String := "");
   --  Generate the alr-session.ads file for current project file
   --  If no alr file, an empty hash will be used so this will be an outdated or stand-alone executable
   --  This is needed when, for example, searching or getting projects: we are outside any project but
   --    need the full index.

   Sed_Pattern : constant String;

private

   procedure Generate_Full_Index (File : in out Ada.Text_IO.File_Type; Index_Folder : String);
   --  Add with Alire.Index.* for all dependencies in the given already opened file


   Sed_Pattern : constant String := "PROJECT_SKEL";
   --  In Caps so its proper use in lowercase or Mixed Case is ensured

end Alr.Templates;
