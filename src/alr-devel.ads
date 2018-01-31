package Alr.Devel with Preelaborate is

   Enabled : constant Boolean := True;
   --  This global switch changes some behaviors to make for a more friendlier development experience:
   --  Rolling behavior is disabled so there's never respawning of the canonical rolling executable
   --  Self-compilation is launched for the project file in the current folder instead of the canonical one

end Alr.Devel;
