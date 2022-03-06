with Alire.Containers;
limited with Alire.Dependencies.Containers;
limited with Alire.Roots.Optional;

package Alire.Install is

   --  Support for installation prefixes

   function Root (Prefix : Any_Path) return Roots.Optional.Root;
   --  Attempt to detect an Alire root inside the given location (which is the
   --  prefix itself, not the expected root location).

   procedure Print (Prefix : Any_Path);

   procedure Add (Prefix : Any_Path;
                  Deps   : Dependencies.Containers.List);
   --  Resolve the dependencies and install the resulting releases. If a
   --  different version is installed it will be uninstalled first. If a
   --  crate is given twice it will raise.

   procedure Remove (Prefix : Any_Path;
                     Crates : Containers.Crate_Name_Sets.Set);
   --  Remove the given crates, or raise if it isn't found in the given prefix

end Alire.Install;
