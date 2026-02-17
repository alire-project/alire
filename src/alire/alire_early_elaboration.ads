with Ada.Calendar;

with Alire;

package Alire_Early_Elaboration with Elaborate_Body is

   --  This body should be elaborated among the first ones.
   --  For anything requiring early elaboration (e.g. logging setup)

   --  Not directly a child of Alire to avoid circularity

   --  Logging in alire works in two separate channels:
   --  The -q, (none), -v and -vv switches allow to select one of the normal
   --  verbosity levels: quiet, normal, verbose, detail.
   --  OTOH, the -d/--debug switch enables logging of all unexpected exceptions
   --  to stderr independently of the verbosity level.

   Switch_D  : aliased Boolean := False;
   --  For the debugging channel.

   Switch_Q,
   Switch_V,
   Switch_VV : aliased Boolean := False;
   --  For the verbosity level.

   Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   --  Out of curiosity

   function Get_Starting_Dir return Alire.Absolute_Path;
   --  The cwd from which `alr` was called, before any -C/--chdir processing

   function Get_Effective_Dir return Alire.Absolute_Path;
   --  The effective starting directory: the one set by -C/--chdir, or else
   --  the one where `alr` was run from. This is used during the latter pro-
   --  cessing of switches, to verify that we are at the intended location.

end Alire_Early_Elaboration;
