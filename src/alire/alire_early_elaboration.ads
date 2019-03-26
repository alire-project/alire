with Ada.Calendar;

package Alire_Early_Elaboration with Elaborate_Body is

   --  This body should be elaborated among the first ones.
   --  For anything requiring early elaboration (e.g. logging setup)

   --  Not directly a child of Alire to avoid circularity

   Switch_D,
   Switch_Q,
   Switch_V : aliased Boolean := False;
   --  Verbosity switches detected during early elaboration

   Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   --  Out of curiosity

end Alire_Early_Elaboration;
