with Alire.Interfaces;
with Alire.Solutions;
with Alire.TOML_Adapters;

with TOML;

package Alire.Lockfiles is

   Simple_Name : constant String := "alire.lock";

   type Validities is (Missing, Invalid, Valid);

   --  The lockfile stores persistent private information read/written by
   --  Alire and not intended for human tinkering. This currently includes
   --  the solution for the root dependencies (that itself includes any pin
   --  overrides).

   type Contents is
     new Interfaces.Detomifiable
     and Interfaces.Tomifiable with
   record
      Solution : Solutions.Solution;
   end record;
   --  Information that goes in the lockfile

   function File_Name (Root_Dir : Absolute_Path) return Absolute_Path;
   --  Return the location /path/to/crate/dir/alire.lock, filename included,
   --  given the root directory where the crate is deployed.

   function Read (Filename : Absolute_Path) return Contents;
   --  Read contents from the given lockfile

   function Validity (File : Any_Path) return Validities;
   --  Check if given file is a valid lockfile

   procedure Write (Contents : Lockfiles.Contents;
                    Filename : Absolute_Path);
   --  Write persistent contents to a file

   overriding
   function From_TOML (This : in out Contents;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome;

   overriding
   function To_TOML (This : Contents) return TOML.TOML_Value;

end Alire.Lockfiles;
