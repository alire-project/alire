private with Alire.Conditional_Trees;
private with Alire.Interfaces;
private with Alire.Platforms;
private with Alire.TOML_Keys;

private with TOML;

package Alire.Externals.From_System is

   --  A system-provided package, installed via a platform-specific method such
   --  as apt, yum, etc. that can also inform us about the version.

   type External is new Externals.External with private;

   overriding
   function Detect (This : External;
                    Name : Crate_Name) return Releases.Containers.Release_Set;

   overriding
   function Image (This : External) return String;

   overriding
   function Detail (This   : External;
                    Distro : Platforms.Distributions)
                    return AAA.Strings.Vector;

   overriding
   function Kind (This : External) return String is ("System package");

   function From_TOML (From : TOML_Adapters.Key_Queue) return External;
   --  From must point to the table with the keys that describe the external.

private

   --  To reuse the conditional expressions parser we need a bit of boilerplate

   type Package_Vector is
     new Interfaces.Classificable
     and Interfaces.Tomifiable
     and Interfaces.Yamlable with record
      Packages : AAA.Strings.Vector;
   end record;

   overriding
   function Key (This : Package_Vector) return String
   is (TOML_Keys.Origin);

   overriding
   function To_TOML (This : Package_Vector) return TOML.TOML_Value
   is (raise Unimplemented); -- Not needed

   overriding
   function To_YAML (This : Package_Vector) return String
   is (raise Unimplemented); -- Not needed

   function Image (This : Package_Vector) return String;

   package Conditional_Packages is new Conditional_Trees (Package_Vector,
                                                          Image);

   type Candidates is array (Platforms.Known_Distributions) of Package_Vector;

   type External is new Externals.External with record
      Origin : Conditional_Packages.Tree;
   end record;

end Alire.Externals.From_System;
