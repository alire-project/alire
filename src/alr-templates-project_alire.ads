with Alr.Project; use Alr.Project;

package Alr.Templates.Project_Alire is 
   
   Working_Release  : constant Release := Set_Root_Project (
      "project",
      V ("0.0.0"),
      License => Alire.Unknown,
      Depends_On => 
         Exactly ("project", V ("1.0.0")));
      
end Alr.Templates.Project_Alire;
