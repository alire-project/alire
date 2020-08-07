with Alire.Dependencies.Containers;

package Alire.Manifest is

   --  Subprograms for manipulation of the manifest file

   procedure Append (Name : Any_Path;
                     Deps : Dependencies.Containers.List);

   procedure Remove (Name : Any_Path;
                     Deps : Dependencies.Containers.List);
   --  Do a best effort to remove dependencies; if we cannot locate a
   --  dependency with enough guarantees of not botching the manifest,
   --  no change will be applied and Checked_Error will be raised.

   function Is_Valid (Name : Any_Path) return Boolean;
   --  Check that the given Name is a loadable manifest

end Alire.Manifest;
