with Alire.Roots.Editable;

package Alire.Environment.Loading is

   procedure Load (This        : in out Context;
                   Root        : in out Alire.Roots.Root;
                   For_Hashing : Boolean := False);
   --  Load the environment variables of a releases found in the workspace
   --  Solution (GPR_PROJECT_PATH and custom variables) in the context. If
   --  For_Hashing, skip or mock actions that require the build hash which is
   --  part of the build path. We use this to gather all configuration when
   --  paths aren't yet known (as they depend on the hash that is computed
   --  from the configuration which will become itself part of the path).

private

   procedure Load (This            : in out Context;
                   Root            : in out Roots.Editable.Root;
                   Crate           : Crate_Name;
                   For_Hashing     : Boolean := False);
   --  Load the environment variables of a release (GPR_PROJECT_PATH and custom
   --  variables) in the context. See note in previous Load about For_Hashing.

end Alire.Environment.Loading;
