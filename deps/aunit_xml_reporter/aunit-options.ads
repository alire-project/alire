------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         A U N I T . O P T I O N S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2009-2013, AdaCore                   --
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

with AUnit.Test_Filters;

package AUnit.Options is

   type AUnit_Options is record
      Global_Timer     : Boolean := False;
      Test_Case_Timer  : Boolean := False;
      Report_Successes : Boolean := True;
      Filter           : AUnit.Test_Filters.Test_Filter_Access := null;
   end record;
   --  Options used to determine how a test should be run.

   Default_Options : constant AUnit_Options :=
     (Global_Timer     => False,
      Test_Case_Timer  => False,
      Report_Successes => True,
      Filter           => null);

end AUnit.Options;
