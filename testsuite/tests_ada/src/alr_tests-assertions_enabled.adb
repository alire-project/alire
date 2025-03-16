procedure Alr_Tests.Assertions_Enabled is
begin
   --  Ensure assertions are enabled and working as expected
   begin
      pragma Assert (False, "should always raise");
   exception
      when others =>
         return; -- Assert raised as expected and we are done
   end;
   raise Program_Error with "assertion was not honored";
end Alr_Tests.Assertions_Enabled;
