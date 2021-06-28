with Alire.Dependencies;
with Alire.User_Pins;

package Alire.Manifest is

   type Sources is (Index, Local);
   --  We may have slightly different mandatory fields for a manifest that is
   --  coming from an index or from a local crate (initialized, pinned...)

   --  Subprograms for manipulation of the manifest file

   procedure Append (Name : Any_Path;
                     Dep  : Dependencies.Dependency);

   procedure Remove (Name : Any_Path;
                     Dep  : Crate_Name);
   --  Do a best effort to remove dependencies; if we cannot locate a
   --  dependency with enough guarantees of not botching the manifest,
   --  no change will be applied and Checked_Error will be raised.

   procedure Append (File  : Any_Path;
                     Crate : Crate_Name;
                     Pin   : User_Pins.Pin);

   procedure Remove_Pin (File : Any_Path;
                         Pin  : Crate_Name);
   --  As removal of dependencies, but for pins. If the pin is not found, or
   --  it cannot be safely removed, it will raise.

   function Is_Valid (Name : Any_Path; Source : Sources) return Boolean;
   --  Check that the given Name is a loadable manifest

end Alire.Manifest;
