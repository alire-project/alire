with Alire.Conditional;
with Alire.Dependencies;
with Alire.Platforms;

package Alire.Toolchains is

   procedure Assistant (Current_OS : Platforms.Operating_Systems);
   --  Runs the interactive assistant to select the default toolchain. By
   --  default, the native Alire-provided compiler for Current_OS is proposed.

   function Compiler_Is_Configured return Boolean;
   --  Say if a compiler is actually configured by the user

   function Compiler_Dependency return Dependencies.Dependency
     with Pre => Compiler_Is_Configured;
   --  Return the configured compiler as an exact compiler=version dependency

   function Add_Compiler (Deps : Conditional.Dependencies)
                          return Conditional.Dependencies;
   --  If none in Deps is a concrete compiler, the user-configured one will
   --  be added as a dependency, if defined. This way, generic dependencies on
   --  gnat will not result in another compiler hosted in the current platform
   --  supplanting the one wanted by the user (otherwise, the solver will
   --  consider them all as valid and pick one of them "randomly"). This is
   --  currently determined by a crate being named "gnat_xxx". To bear in mind
   --  if we end indexing something with such a name that is not a compiler.
   --  Alternatively, we could define a new labeled property for the manifest.

end Alire.Toolchains;
