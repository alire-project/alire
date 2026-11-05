package Alire.Roots.Probes is

   --  Find out what gprbuild uses in a root, by having it do it for real

   type Ada_Compiler_Probe (Found : Boolean := False) is record
      case Found is
         when True =>
            Driver : Unbounded_Absolute_Path;
            --  The compiler gprbuild runs
         when False =>
            Reason : UString;
            --  Why the compiler could not be determined
      end case;
   end record;

   function Ada_Compiler (This         : in out Root;
                          Project_File : Relative_File)
                          return Ada_Compiler_Probe;
   --  Identifies the compiler used for a project file of the given Root. This
   --  is achieved by compiling a stub spec as part of the project thanks to
   --  gprbuild arguments to this effect. Since the actual build is driven by
   --  gprbuild, it takes into account all factors that can modify its choice,
   --  not matter what alr has tried to set up in the environment.

end Alire.Roots.Probes;
