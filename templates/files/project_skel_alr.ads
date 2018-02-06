with Alr.Project; use Alr.Project;

package TMPL_PROJECT_MC_Alr is

   Working_Release : constant Release := Set_Root_Project (
      Name => "TMPL_PROJECT_LC",
      Depends_On =>
         TMPL_SEMRANGE ("alr", V ("TMPL_VERSION")));

   --  An explicit dependency on alr is only needed if you want to compile this file.
   --  To do so, include the "alr.gpr" project in your own project file.
   --  Once you are satisfied with your own dependencies, it can be safely removed.

end TEMPL_PROJECT_MC_Alr;
