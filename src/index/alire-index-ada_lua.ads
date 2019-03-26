with Alire.Index.Liblua;

package Alire.Index.Ada_Lua is

   function Project is new Catalogued_Project ("An Ada binding for Lua");

   Prj_Repo       : constant URL    := "https://github.com/alire-project/ada-lua.git";
   Prj_Maintainer : constant String := "AdaCore";
   Prj_Website    : constant URL    := "https://github.com/AdaCore/ada-lua";

   V_0_0_0 : constant Release :=
               Project.Register
                 (V ("0.0.0-5.3"),
                  Git (Prj_Repo, "ba2fcbf9f8d54d3f6362f20523deb4371371f658"),

                  Dependencies       =>
                    Liblua.V_5_3.Within_Major,

                  Properties         =>
                    Project_File ("lua.gpr") and

                    Executable ("main") and

                    Maintainer (Prj_Maintainer) and
                    Website    (Prj_Website) and
                    License    (GPL_3_0),

                  Private_Properties =>
                    Project_File ("examples/example1/example1.gpr") and
                    Project_File ("examples/example2/example2.gpr")
                 );

end Alire.Index.Ada_Lua;
