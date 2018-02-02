with Alr.Project; use Alr.Project;

package Alr.Templates.Project_Alire is 
   
   Working_Release  : constant Release := Set_Root_Project (
      "project",
      V ("0.0.0"),
      License => Alire.Unknown,
      Depends_On => 
      Exactly ("project", V ("1.0.0")));
   
   --  The explicit dependency on alr is only needed if you want to compile this file.
   --  Once you are satisfied with your own dependencies it can be safely removed.
      
end Alr.Templates.Project_Alire;
