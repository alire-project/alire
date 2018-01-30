with Alire.Index; use Alire.Index;

package Alr.Templates.Project_Alire is 

   Name : constant Alire.Project_Name := "project_name";
   
   V_1_0_0  : constant Release := 
                Register_Local (Name,
                                V ("1.0.0"),
                                License => Alire.Unknown,
                                Depends_On => Exactly ("project", V ("1.0.0")));

end Alr.Templates.Project_Alire;
