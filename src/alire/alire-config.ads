with Alire.OS_Lib; use Alire.OS_Lib.Operators;

package Alire.Config is

   --  TODO: refactor this globals package into a type that can be
   --  passed around.

   --  For Alire to be usable as a library, options here aren't parsed
   --  from command-line but set by someone else that has done the parsing.
   --  Right now the only client is alr.

   -----------
   -- Paths --
   -----------

   function Path return String;
   --  The in-use config path.
   --  In order of decreasing precedence:
   --  * A manually set path with Set_Path (below)
   --  * An ALR_CONFIG env given folder
   --  * Default per-platform path (see alire-platforms-*)

   --  Detection happens during elaboration.

   procedure Set_Path (Path : String);
   --  Override

   function Indexes_Directory return Absolute_Path is (Path / "indexes");

   -------------------
   -- Interactivity --
   -------------------

   Not_Interactive : aliased Boolean := False;
   --  When not Interactive, instead of asking the user something, use default.
   --  Currently only used before the first call to `sudo apt` to ask for
   --  confirmation.
   --  TODO: remove global eventually

   procedure Enter_Or_Ctrl_C;
   --  If interactive, ask the user to press Enter or Ctrl-C to stop.
   --  Output a log trace otherwise and continue.

   --------------
   -- Symlinks --
   --------------

   Use_Symlinks : aliased Boolean := False;
   --  When possible, use symbolic links for local crates instead of copying
   --  the source directory.

end Alire.Config;
