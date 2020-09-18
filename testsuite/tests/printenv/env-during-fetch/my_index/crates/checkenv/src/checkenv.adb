with GNAT.IO;     use GNAT.IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

procedure Checkenv is
begin
   --  Check that "CHECKENV_TEST_VAR" is defined and print message accordingly
   --  to stderr
   if Getenv ("CHECKENV_TEST_VAR").all /= "" then
      Put_Line (Standard_Error, "CHECKENV_TEST_VAR exists");
   else
      Put_Line (Standard_Error, "CHECKENV_TEST_VAR does NOT exist");
   end if;
end Checkenv;
