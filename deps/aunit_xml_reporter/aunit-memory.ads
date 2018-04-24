------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         A U N I T . M E M O R Y                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2008-2011, AdaCore                     --
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

--  Provides the memory handling mechanism used by AUnit. This allows in
--  particular AUnit to use dynamic allocation even on limited run-times that
--  do not provide memory management.
--  See also AUnit.Memory.Utils that provides an easy to use allocator for
--  complex objects.

with System;

package AUnit.Memory is

   type size_t is mod 2 ** Standard'Address_Size;

   function AUnit_Alloc (Size : size_t) return System.Address;

   procedure AUnit_Free (Obj : System.Address);

private

   pragma Inline (AUnit_Alloc);
   pragma Inline (AUnit_Free);

end AUnit.Memory;
