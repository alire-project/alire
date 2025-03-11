procedure Alr_Tests.Assertions_Enabled is
begin
   --  Ensure assertions are working as expected
   pragma Assert (False);
   raise Program_Error with "assertion was not honored";
exception
   when others =>
      null;
end Alr_Tests.Assertions_Enabled;
