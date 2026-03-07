with Alire.Utils;

procedure Alr_Tests.Format_Duration is
   function F (D : Duration) return String renames Utils.Format_Duration;
begin
   --  small values
   pragma Assert (F (0.0) = "0s000");
   pragma Assert (F (0.5) = "0s500");

   --  rounding normal values
   pragma Assert (F (12.679) = "12s679");
   pragma Assert (F (12.6799) = "12s680");

   --  seconds/minutes boundary
   pragma Assert (F (59.999) = "59s999");
   pragma Assert (F (59.9994) = "59s999");
   pragma Assert (F (59.9995) = "1m00");
   pragma Assert (F (60.0) = "1m00");

   --  minutes/hours boundary
   pragma Assert (F (3599.0) = "59m59");
   pragma Assert (F (3599.4) = "59m59");
   pragma Assert (F (3599.5) = "1h00");
   pragma Assert (F (3600.0) = "1h00");

   --  last value
   pragma Assert (F (Duration'Last) = "2562047h47");

   --  negative values
   pragma Assert (F (-0.5) = "-" & F (0.5));
   pragma Assert (F (-12.679) = "-" & F (12.679));
   pragma Assert (F (-60.0) = "-" & F (60.0));
   pragma Assert (F (-3601.0) = "-" & F (3601.0));
end Alr_Tests.Format_Duration;
