------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     A U N I T . T I M E _ M E A S U R E                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                    Copyright (C) 2006-2014, AdaCore                      --
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

with Ada.Calendar;

package AUnit.Time_Measure is

   type Time is record
      Start : Ada.Calendar.Time;
      Stop  : Ada.Calendar.Time;
   end record;

   type AUnit_Duration is private;

   Null_Time : constant Time := (Start => Ada.Calendar.Time_Of (1901, 1, 1),
                                 Stop  => Ada.Calendar.Time_Of (1901, 1, 1));

   procedure Start_Measure (T : in out Time);
   --  Start a new measure

   procedure Stop_Measure (T : in out Time);
   --  Stop the measure

   function Get_Measure (T : Time) return AUnit_Duration;
   --  Get the measure

   generic
      with procedure Put (I : Integer) is <>;
      with procedure Put (S : String) is <>;
   procedure Gen_Put_Measure (Measure : AUnit_Duration);
   --  Put the image of the measure

   generic
      with procedure Put (I : Integer) is <>;
      with procedure Put (S : String) is <>;
   procedure Gen_Put_Measure_In_Seconds (Measure : AUnit_Duration);
   --  Unlike Gen_Put_Measure, puts the measure in seconds only, also puts
   --  9 digits after decimal point.

private

   type AUnit_Duration is new Duration;

end AUnit.Time_Measure;
