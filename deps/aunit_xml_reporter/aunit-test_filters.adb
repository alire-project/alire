------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          A U N I T . T E S T S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2009-2011, AdaCore                       --
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
-- GNAT is maintained by AdaCore (http://www.adacore.com).                  --
--                                                                          --
------------------------------------------------------------------------------

with AUnit.Simple_Test_Cases;  use AUnit.Simple_Test_Cases;

package body AUnit.Test_Filters is

   function Starts_With (Str : String; Prefix : String) return Boolean;
   --  Whether Str starts with Prefix

   -----------------
   -- Starts_With --
   -----------------

   function Starts_With (Str : String; Prefix : String) return Boolean is
   begin
      if Str'Length < Prefix'Length then
         return False;
      end if;

      return Str (Str'First .. Str'First + Prefix'Length - 1) = Prefix;
   end Starts_With;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Filter : in out Name_Filter; Name : String) is
   begin
      Message_Free (Filter.Name);
      Filter.Name := Format (Name);
   end Set_Name;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active
     (Filter : Name_Filter;
      T      : AUnit.Tests.Test'Class) return Boolean is
   begin
      if Filter.Name = null
        or else Filter.Name.all = ""
      then
         return True;
      end if;

      if T not in AUnit.Simple_Test_Cases.Test_Case'Class
        or else Name (AUnit.Simple_Test_Cases.Test_Case'Class (T)) = null
      then
         --  There is no name, so it doesn't match the filter
         return False;
      end if;

      if Routine_Name (AUnit.Simple_Test_Cases.Test_Case'Class (T)) = null then
         return Starts_With
           (Name (AUnit.Simple_Test_Cases.Test_Case'Class (T)).all,
            Filter.Name.all);
      else
         return Starts_With
           (Name (AUnit.Simple_Test_Cases.Test_Case'Class (T)).all
            & " : "
            & Routine_Name (AUnit.Simple_Test_Cases.Test_Case'Class (T)).all,
            Filter.Name.all);
      end if;
   end Is_Active;

end AUnit.Test_Filters;
