procedure Alr_Tests.Assertions_Enabled is
begin
   --  Ensure assertions are enabled and working as expected
   pragma Assert (False, "should always raise");
   raise Program_Error with "assertion was not honored";
exception
   when others =>
      null; -- Assert raised as expected and we are done
end Alr_Tests.Assertions_Enabled;
