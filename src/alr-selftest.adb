with Ada.Exceptions;
with Ada.Text_IO;

with AJUnitGen;

with GNAT.Source_Info;

package body Alr.Selftest is

   Suite : AJUnitGen.Test_Suite := AJUnitGen.New_Suite ("selftest");

   generic
      with procedure Test;
   package Unit_Test is
   end Unit_Test;

   package body Unit_Test is
   begin
      Test;

      Suite.Add_Case (AJUnitGen.New_Case (GNAT.Source_Info.Enclosing_Entity));
   exception
      when E : others =>
         Suite.Add_Case (AJUnitGen.New_Case
                         (GNAT.Source_Info.Enclosing_Entity,
                            AJUnitGen.Fail,
                            "FAIL",
                            Ada.Exceptions.Exception_Message (E),
                            Ada.Exceptions.Exception_Information (E)));
   end Unit_Test;

--     procedure Fail is
--     begin
--        raise Constraint_Error;
--     end Fail;

   procedure Nothing is null;

   procedure Run is
      use Ada.Text_IO;
      F : File_Type;

--        package Test_Fail is new Unit_Test (Fail) with Unreferenced;
      package Test_Nothing is new Unit_Test (Nothing) with Unreferenced;
   begin
      Trace.Detail ("Tests completed");

      Create (F, Out_File, "alr_selftest.xml");
      Suite.To_Collection.Write (F);
      Close (F);
   end Run;

end Alr.Selftest;
