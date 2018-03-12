package Alr.Native is

   --  DEPRECATED
   --  Autodetection not in use

   --  Stuff related to platform native packages

   procedure Autodetect (Force : Boolean := False);
   --  Detects (if not already done) native candidates

   procedure Add_To_Index;
   --  Add any found native packages to index

end Alr.Native;
