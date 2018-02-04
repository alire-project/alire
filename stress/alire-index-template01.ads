with Alire.Index.Libhello;

package Alire.Index.Template is

   Name : constant Project_Name := "Template";
   Repo : constant URL		:= "https://bitbucket.org/aleteolabs/hello.git";

   V_1_0_0  : constant Release :=
                Register_Git (Name,
                              V ("1.0.0"),
                              Repo,
                              "8cac0afddc505794ae3e5634745ce0830129d241",
                              Depends_On => At_Least_Within_Major (Libhello.V_1_0_0));

end Alire.Index.Template;
