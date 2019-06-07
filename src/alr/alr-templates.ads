with Ada.Text_IO;

with Alire.Roots;

with Alr.Query;

package Alr.Templates is

   type Generation_Scenarios is
     (Released,  -- Project is already released and packaged
      Unreleased -- Project is a working copy, pinned or some other variation
      --  with explicit dependencies
     );
   --  In initial generation we know the only dependency is on Alire itself
   --  When pinning we are fixing to current resolved versions
   --  Otherwise who knows

   procedure Generate_Agg_Gpr (Instance : Query.Instance;
                               Root     : Alire.Roots.Root);
   --  Generate the aggregate project file with given resolved dependencies

   procedure Generate_Agg_Gpr (Root : Alire.Roots.Root);
   --  Generate the aggregate project file solving the dependencies of the
   --  given root.

   procedure Generate_Prj_Alr (Release  : Types.Release;
                               Filename : String);
   --  Generate textual release representation at given location

   procedure Generate_Prj_Alr (Release : Types.Release);
   --  As previous, but ensure that we are at the project root folder

   Sed_Pattern : constant String;

private

   procedure Generate_Full_Index (File         : in out Ada.Text_IO.File_Type;
                                  Index_Folder : String);
   --  Add with Alire.Index.* for all dependencies in the given already opened
   --  file.

   Sed_Pattern : constant String := "PROJECT_SKEL";
   --  In Caps so its proper use in lowercase or Mixed Case is ensured

end Alr.Templates;
