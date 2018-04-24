------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--          A U N I T . T E S T _ C A S E S . R U N _ R O U T I N E         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                    Copyright (C) 2006-2011, AdaCore                      --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT is maintained by AdaCore (http://www.adacore.com)                   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;          use Ada.Exceptions;

with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;

with AUnit.Time_Measure;

separate (AUnit.Simple_Test_Cases)

--  Version for run-time libraries that support exception handling
procedure Run_Routine
  (Test    : access Test_Case'Class;
   Options :        AUnit.Options.AUnit_Options;
   R       : in out Result'Class;
   Outcome :    out Status)
is
   Unexpected_Exception : Boolean := False;
   Time : Time_Measure.Time := Time_Measure.Null_Time;

   use Time_Measure;

begin

   --  Reset failure list to capture failed assertions for one routine

   Clear_Failures (Test.all);

   begin
      if Options.Test_Case_Timer then
         Start_Measure (Time);
      end if;

      Run_Test (Test.all);

      if Options.Test_Case_Timer then
         Stop_Measure (Time);
      end if;

   exception
      when Assertion_Error =>
         if Options.Test_Case_Timer then
            Stop_Measure (Time);
         end if;

      when E : others =>
         if Options.Test_Case_Timer then
            Stop_Measure (Time);
         end if;

         Unexpected_Exception := True;
         Add_Error
           (R,
            Name (Test.all),
            Routine_Name (Test.all),
            Error => (Exception_Name    => Format (Exception_Name (E)),
                      Exception_Message => Format (Exception_Message (E)),
                      Traceback         => Format (Symbolic_Traceback (E))),
            Elapsed => Time);
   end;

   if not Unexpected_Exception and then not Has_Failures (Test.all) then
      Outcome := Success;
      Add_Success (R, Name (Test.all), Routine_Name (Test.all), Time);
   else
      Outcome := Failure;
      declare
         C : Failure_Iter := First_Failure (Test.all);
      begin
         while Has_Failure (C) loop
            Add_Failure (R,
                         Name (Test.all),
                         Routine_Name (Test.all),
                         Get_Failure (C),
                         Time);
            Next (C);
         end loop;
      end;
   end if;

end Run_Routine;
