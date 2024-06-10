with Alire.Releases;

package Alire.Crate_Configuration.Hashes is

   procedure Add_From (Config : Global_Config;
                       Rel    : Releases.Release;
                       Add    : access procedure (Kind, Key, Value : String));
   --  Add configuration values of the given crate to the build hash input

end Alire.Crate_Configuration.Hashes;
