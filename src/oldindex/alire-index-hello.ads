with Alire.Index.Libhello;

package Alire.Index.Hello is

   function Project is
     new Catalogued_Project ("""Hello, world!"" demonstration project");

   Repo : constant URL := "https://github.com/alire-project/hello.git";

   V_1_0_0  : constant Release :=
                Project.Register
                  (V ("1.0.0"),
                   Git (Repo, "8cac0afddc505794ae3e5634745ce0830129d241"),
                   Dependencies => Libhello.V_1_0.Within_Major);

   V_1_0_1  : constant Release :=
                Project.Register
                  (V_1_0_0.Upgrading
                     (V ("1.0.1"),
                      Git (Repo, "65725c20778875eef12b61a01b437120932965f3")));

end Alire.Index.Hello;
