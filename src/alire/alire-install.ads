limited with Alire.Dependencies.Containers;
with Alire.Directories; use Alire.Directories.Operators;
private with Alire.Milestones.Containers;
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

   Metadata_Dir_In_Prefix : constant Relative_Path
     := "share" / "gpr" / "manifests";
   --  This is used by gprinstall and we will reuse it for our "fake" binary
   --  installs.

   subtype Installed_Milestones is Milestones.Containers.Maps.Map;

   function Find_Installed (Prefix : Any_Path)
                            return Installed_Milestones;
   --  Identify installed releases in the prefix

end Alire.Install;
