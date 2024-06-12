limited with Alire.Dependencies.Containers;
with Alire.Directories; use Alire.Directories.Operators;
with Alire.Milestones.Containers;
with Alire.Platforms.Folders;
with Alire.Releases;

package Alire.Install is

   --  Support for installation prefixes

   Default_Prefix_Basename : constant Relative_Path := ".alire";
   Default_Prefix_Dirname  : constant Absolute_Path := Platforms.Folders.Home;

   Default_Prefix : constant Absolute_Path :=
                      Default_Prefix_Dirname / Default_Prefix_Basename;

   procedure Add (Prefix : Any_Path;
                  Deps   : Dependencies.Containers.List);
   --  Resolve the dependencies and install the resulting releases. If a crate
   --  is given twice it will raise.

   type Actions is (
                    New_Install, -- no conflict
                    Reinstall,   -- install same version again
                    Replace,     -- new version of already installed executable
                    Skip         -- skip install
                   );

   function Check_Conflicts (Prefix : Any_Path;
                             Rel    : Releases.Release)
                             return Actions;

   procedure Info (Prefix : Any_Path);
   --  Display information about the given prefix

   subtype Installed_Milestones is Milestones.Containers.Sets.Set;

   function Find_Installed (Prefix : Any_Path)
                            return Installed_Milestones;
   --  Identify installed releases in the prefix

   function Find_Installed (Prefix : Any_Path;
                            Crate  : Crate_Name)
                            return Installed_Milestones;
   --  Return milestones for only the given crate

   procedure Set_Installed (Prefix : Any_Path; Mil : Milestones.Milestone);
   --  Stores an empty file in share/gpr/manifests/crate=version

   procedure Set_Not_Installed (Prefix : Any_Path; Crate : Crate_Name);
   --  Any and all versions will be marked as not installed. Intended for when
   --  reinstalling a different executable crate version, as only one can be
   --  installed.

private

   Metadata_Dir_In_Prefix : constant Relative_Path
     := "share" / "gpr" / "manifests";
   --  This is used by gprinstall and we will reuse it for our "fake" binary
   --  installs.

end Alire.Install;
