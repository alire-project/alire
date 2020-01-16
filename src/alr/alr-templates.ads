with Alire.Roots;

package Alr.Templates is

   type Generation_Scenarios is
     (Released,  -- Release is already released and packaged
      Unreleased -- Release is a working copy, pinned or some other variation
      --  with explicit dependencies
     );
   --  In initial generation we know the only dependency is on Alire itself
   --  When pinning we are fixing to current resolved versions
   --  Otherwise who knows

   procedure Generate_Prj_Alr (Release  : Types.Release;
                               Filename : String);
   --  Generate textual release representation at given location

   procedure Generate_Prj_Alr (Release : Types.Release);
   --  As previous, but ensure that we are at the working root folder

   Sed_Pattern : constant String;

private

   Sed_Pattern : constant String := "PROJECT_SKEL";
   --  In Caps so its proper use in lowercase or Mixed Case is ensured

end Alr.Templates;
