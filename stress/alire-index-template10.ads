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
   V_1_0_1  : constant Release :=
                Register_Git (Name,
                              V ("1.0.1"),
                              Repo,
                              "8cac0afddc505794ae3e5634745ce0830129d241",
                              Depends_On => At_Least_Within_Major (Libhello.V_1_0_0));
   V_1_0_2  : constant Release :=
                Register_Git (Name,
                              V ("1.0.2"),
                              Repo,
                              "8cac0afddc505794ae3e5634745ce0830129d241",
                              Depends_On => At_Least_Within_Major (Libhello.V_1_0_0));
   V_1_0_3  : constant Release :=
                Register_Git (Name,
                              V ("1.0.3"),
                              Repo,
                              "8cac0afddc505794ae3e5634745ce0830129d241",
                              Depends_On => At_Least_Within_Major (Libhello.V_1_0_0));
   V_1_0_4  : constant Release :=
                Register_Git (Name,
                              V ("1.0.4"),
                              Repo,
                              "8cac0afddc505794ae3e5634745ce0830129d241",
                              Depends_On => At_Least_Within_Major (Libhello.V_1_0_0));
   V_1_0_5  : constant Release :=
                Register_Git (Name,
                              V ("1.0.5"),
                              Repo,
                              "8cac0afddc505794ae3e5634745ce0830129d241",
                              Depends_On => At_Least_Within_Major (Libhello.V_1_0_0));
   V_1_0_6  : constant Release :=
                Register_Git (Name,
                              V ("1.0.6"),
                              Repo,
                              "8cac0afddc505794ae3e5634745ce0830129d241",
                              Depends_On => At_Least_Within_Major (Libhello.V_1_0_0));
   V_1_0_7  : constant Release :=
                Register_Git (Name,
                              V ("1.0.7"),
                              Repo,
                              "8cac0afddc505794ae3e5634745ce0830129d241",
                              Depends_On => At_Least_Within_Major (Libhello.V_1_0_0));
   V_1_0_8  : constant Release :=
                Register_Git (Name,
                              V ("1.0.8"),
                              Repo,
                              "8cac0afddc505794ae3e5634745ce0830129d241",
                              Depends_On => At_Least_Within_Major (Libhello.V_1_0_0));
   V_1_0_9  : constant Release :=
                Register_Git (Name,
                              V ("1.0.9"),
                              Repo,
                              "8cac0afddc505794ae3e5634745ce0830129d241",
                              Depends_On => At_Least_Within_Major (Libhello.V_1_0_0));

end Alire.Index.Template;
