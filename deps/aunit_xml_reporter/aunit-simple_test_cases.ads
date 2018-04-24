------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                A U N I T . S I M P L E _ T E S T _ C A S E S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                        Copyright (C) 2008-2011, AdaCore                  --
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

--  This type is used to implement a simple test case: define a derived type
--  that overrides the Run_Test and Name methods.
--
--  You don't usually need to use that type, but Test_Fixture/Test_Caller
--  or Test_Case instead.

with AUnit.Assertions;
with AUnit.Options;
with AUnit.Test_Results; use AUnit.Test_Results;

package AUnit.Simple_Test_Cases is

   type Test_Case is abstract new AUnit.Assertions.Test with private;
   type Test_Case_Access is access all Test_Case'Class;

   function Name (Test : Test_Case) return Message_String is abstract;
   --  Test case name

   function Routine_Name (Test : Test_Case) return Message_String;
   --  Routine name. By default return a null Message_String

   procedure Run_Test
     (Test          : in out Test_Case) is abstract;
   --  Perform the test.

   procedure Set_Up (Test : in out Test_Case);
   --  Set up performed before each test

   procedure Tear_Down (Test : in out Test_Case);
   --  Tear down performed after each test

   ----------------------------------------------
   --  Below are internal routines. Do not use --
   ----------------------------------------------

   procedure Run (Test    : access Test_Case;
                  Options :        AUnit.Options.AUnit_Options;
                  R       : in out Result'Class;
                  Outcome :    out Status);
   --  Run test case. Do not override

private

   type Test_Case is abstract new AUnit.Assertions.Test with null record;

end AUnit.Simple_Test_Cases;
