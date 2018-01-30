with Alire.Index; use Alire.Index; -- Unneeded in real one, which child of Alire.Index

package Alr.Templates.Project_Alire is 
   
   Working_Release  : constant Release := Register_Local (
      "project",
      V ("0.0.0"),
      License => Alire.Unknown,
      Depends_On => 
         Exactly ("project", V ("1.0.0")));

end Alr.Templates.Project_Alire;
