pragma Alire_Test;
--  Declares this parameterless main procedure as a test for `alr test`. Test
--  mains must opt in like this; a non-test main can opt out instead with
--  `pragma Alire_Test (Auxiliary_File);`. See the `alr test` documentation for
--  the available Alire_Test configuration keys.

procedure @_CAPITALIZE:NAME_@_Tests.Assertions_Enabled is
begin
   begin
      pragma Assert (False, "Should raise");
   exception
      when others =>
         return; -- properly raised
   end;
   raise Program_Error with "Assertion did not raise";
end @_CAPITALIZE:NAME_@_Tests.Assertions_Enabled;
