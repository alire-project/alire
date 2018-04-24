------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--            A D A _ C O N T A I N E R S . A U N I T _ L I S T S           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2008, Free Software Foundation, Inc.         --
--                   Copyright (C) 2008-2011, AdaCore                       --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

--  This unit is an adaptation of the Ada2005 runtime's
--  Ada.Containers.Doubly_Linked_Lists with modifications to make this package
--  compatible with the ZFP profiles and also compatible with AUnit's memory
--  management.

--  Some Ada2005 constructs have also been removed so that user tests can be
--  compiled in Ada95.

generic
   type Element_Type is private;

   with function "=" (Left, Right : Element_Type)
      return Boolean is <>;

package Ada_Containers.AUnit_Lists is

   type List is tagged limited private;

   type Cursor is private;

--    Empty_List : constant List;

   No_Element : constant Cursor;

   function "=" (Left, Right : List) return Boolean;

   function Length (Container : List) return Count_Type;

   function Is_Empty (Container : List) return Boolean;

   procedure Clear (Container : in out List);

   function Element (Position : Cursor) return Element_Type;

   procedure Replace_Element
     (Container : in out List;
      Position  : Cursor;
      New_Item  : Element_Type);

--     procedure Query_Element
--       (Position : Cursor;
--        Process  : not null access procedure (Element : Element_Type));

--     procedure Update_Element
--       (Container : in out List;
--        Position  : Cursor;
--      Process   : not null access procedure (Element : in out Element_Type));

   procedure Move
     (Target : in out List;
      Source : in out List);

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type := 1);

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Count     : Count_Type := 1);

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      Position  : out Cursor;
      Count     : Count_Type := 1);

   procedure Prepend
     (Container : in out List;
      New_Item  : Element_Type;
      Count     : Count_Type := 1);

   procedure Append
     (Container : in out List;
      New_Item  : Element_Type;
      Count     : Count_Type := 1);

   procedure Delete
     (Container : in out List;
      Position  : in out Cursor;
      Count     : Count_Type := 1);

   procedure Delete_First
     (Container : in out List;
      Count     : Count_Type := 1);

   procedure Delete_Last
     (Container : in out List;
      Count     : Count_Type := 1);

   procedure Reverse_Elements (Container : in out List);

   procedure Swap
     (Container : in out List;
      I, J      : Cursor);

   procedure Swap_Links
     (Container : in out List;
      I, J      : Cursor);

   procedure Splice
     (Target : in out List;
      Before : Cursor;
      Source : in out List);

   procedure Splice
     (Target   : in out List;
      Before   : Cursor;
      Source   : in out List;
      Position : in out Cursor);

   procedure Splice
     (Container : in out List;
      Before    : Cursor;
      Position  : Cursor);

   function First (Container : List) return Cursor;

   function First_Element (Container : List) return Element_Type;

   function Last (Container : List) return Cursor;

   function Last_Element (Container : List) return Element_Type;

   function Next (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   function Previous (Position : Cursor) return Cursor;

   procedure Previous (Position : in out Cursor);

   function Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor;

   function Reverse_Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor;

   function Contains
     (Container : List;
      Item      : Element_Type) return Boolean;

   function Has_Element (Position : Cursor) return Boolean;

   type Iterator is access procedure (Position : Cursor);

   procedure Iterate
     (Container : List;
      Process   : Iterator);

--     procedure Reverse_Iterate
--       (Container : List;
--        Process   : not null access procedure (Position : Cursor));

   generic
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   package Generic_Sorting is

      function Is_Sorted (Container : List) return Boolean;

      procedure Sort (Container : in out List);

      procedure Merge (Target, Source : in out List);

   end Generic_Sorting;

private

   pragma Inline (Next);
   pragma Inline (Previous);

   type Node_Type;
   type Node_Access is access Node_Type;
   pragma No_Strict_Aliasing (Node_Access);

   type Node_Type is limited record
      Element : Element_Type;
      Next    : Node_Access;
      Prev    : Node_Access;
   end record;

   type List is tagged limited record
      First  : Node_Access := null;
      Last   : Node_Access := null;
      Length : Count_Type := 0;
      Busy   : Natural := 0;
      Lock   : Natural := 0;
   end record;

   type List_Access is access constant List;

   type Cursor is record
      Container : List_Access;
      Node      : Node_Access;
   end record;

   --  Empty_List : constant List := (Controlled with null, null, 0, 0, 0);

   No_Element : constant Cursor := (null, null);

end Ada_Containers.AUnit_Lists;
