limited with Alire.Dependencies.Containers;
with Alire.Directories; use Alire.Directories.Operators;
with Alire.Platforms.Folders;

package Alire.Install is

   --  Support for installation prefixes

   Default_Prefix : constant Absolute_Path
     := Platforms.Folders.Home / ".alire";

   procedure Add (Prefix : Any_Path;
                  Deps   : Dependencies.Containers.List);
   --  Resolve the dependencies and install the resulting releases. If a
   --  crate is given twice it will raise.

   procedure Info (Prefix : Any_Path);
   --  Display information about the given prefix

private

   Gnatinstall_Metadata_Dir_In_Prefix : constant Relative_Path
     := "share" / "gpr" / "manifests";
   --  Installations performed with gprinstall

   Alire_Metadata_Dir_In_Prefix : constant Relative_Path
     := "share" / "alire" / "gprless";
   --  Installations performed by directly copying a binary release

end Alire.Install;
