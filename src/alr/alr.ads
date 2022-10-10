private with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with Alire;

with Simple_Logging;
with CLIC.TTY;

package Alr with Preelaborate is

   --  Nothing of note in this root package. Entities declared here are
   --  generally useful everywhere or in many packages: Exceptions for
   --  commands, tracing for all

   Child_Failed   : exception;
   --  Used to notify that a subprocess completed with non-zero error

   Command_Failed : exception;
   --  Signals "normal" command completion with failure (i.e., no need to print
   --  stack trace).

   --  Use some general types for the benefit of all child packages:
   pragma Warnings (Off, "has no effect");
   use all type Alire.Crate_Name;
   use all type Simple_Logging.Levels;
   pragma Warnings (On);

   package Trace renames Simple_Logging;
   package TTY renames CLIC.TTY;

   function "+" (S : Alire.UString) return String
                 renames Alire.UStrings.To_String;

   --  Some Paths constants that help to break circularities

   Bootstrap_Hash : constant String := "bootstrap";

private

   function WW (S : Ada.Strings.UTF_Encoding.UTF_8_String)
                return Wide_Wide_String
                renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings.decode;

end Alr;
