with Alire.Interfaces;
with Alire.Properties;
with Alire.Solutions;
with Alire.TOML_Adapters;
with Alire.Utils;

with TOML;

package Alire.Lockfiles is

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

   function Read (Filename : Any_Path) return Contents;
   --  Read contents from the given toml file

   function Validity (File : Any_Path) return Validities;
   --  Check if given file is a valid lockfile

   procedure Write (Contents : Lockfiles.Contents;
                    Filename : Any_Path);
   --  Append/replace lockfile contents to the given toml file

   overriding
   function From_TOML (This : in out Contents;
                       From :        TOML_Adapters.Key_Queue)
                       return Outcome;

   overriding
   function To_TOML (This : Contents) return TOML.TOML_Value;

end Alire.Lockfiles;
