------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                A U N I T . S I M P L E _ T E S T _ C A S E S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                        Copyright (C) 2008-2012, AdaCore                  --
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

with AUnit.Assertions;   use AUnit.Assertions;
with AUnit.Options;      use AUnit.Options;
with AUnit.Test_Filters; use AUnit.Test_Filters;

package body AUnit.Simple_Test_Cases is

   procedure Run_Routine
     (Test    : access Test_Case'Class;
      Options :        AUnit_Options;
      R       : in out Result'Class;
      Outcome :    out Status);
   --  Run one test routine

   -----------------
   -- Run_Routine --
   -----------------

   procedure Run_Routine
     (Test    : access Test_Case'Class;
      Options :        AUnit_Options;
      R       : in out Result'Class;
      Outcome :    out Status) is separate;

   ------------------
   -- Routine_Name --
   ------------------

   function Routine_Name (Test : Test_Case) return Message_String is
      pragma Unreferenced (Test);
   begin
      return null;
   end Routine_Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (Test : in out Test_Case) is
      pragma Unreferenced (Test);
   begin
      null;
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (Test : in out Test_Case) is
      pragma Unreferenced (Test);
   begin
      null;
   end Tear_Down;

   ---------
   -- Run --
   ---------

   procedure Run
     (Test    : access Test_Case;
      Options :        AUnit_Options;
      R       : in out Result'Class;
      Outcome :    out Status)
   is
      Old : constant Test_Access := AUnit.Assertions.Current_Test;
   begin
      Outcome := Success;
      if Options.Filter = null
        or else Is_Active (Options.Filter.all, Test.all)
      then
         AUnit.Assertions.Set_Current_Test (Test_Access (Test));
         Init_Test (Test.all);
         Start_Test (R, 1);

         --  Run test routine
         Set_Up (Test_Case'Class (Test.all));
         Run_Routine (Test, Options, R, Outcome);
         Tear_Down (Test_Case'Class (Test.all));
         AUnit.Assertions.Set_Current_Test (Old);
      end if;
   end Run;

end AUnit.Simple_Test_Cases;
