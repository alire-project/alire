with Ada.Calendar;

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

end Alire_Early_Elaboration;
