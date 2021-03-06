private with Ada.Containers.Indefinite_Ordered_Maps;

with Alire.Errors;

generic
   type Elements (<>) is private;
package Alire.Expressions.Maps with Preelaborate is

   type Map is tagged private;
   --  The main operation we need in our index expressions is to look up a
   --  value from the Variable and get the associated value (a dependency,
   --  a property...). These Maps replace the old arrays over a real enum.

   function Empty (V : Variable) return Map;
   --  Initialize a map for a particular type, containing no mapping

   function Base (M : Map) return Variable;
   --  Retrieve the type for which this Map was declared

   function Contains (M : Map; V : String) return Boolean;

   function Element (M : Map; V : String) return Elements with
     Pre => M.Contains (V) or else M.Has_Others or else
     raise Checked_Error with
       Errors.Set ("Map for " & M.Base.Name
                   & " does not have a value for " & V);
   --  Get an element from the map

   type Key_Array is array (Positive range <>) of UString;

   function Keys (M              : Map;
                  Ada_Like       : Boolean;
                  Exclude_Others : Boolean)
                  return Key_Array;
   --  Lazy solution to avoid full-fledged iteration. We don't expect to
   --  have more than a few keys anyway. When Ada_Like, "..." is returned as
   --  "others" instead. When Exclude_Others, only explicit keys are returned.

   function Other (M : Map) return Elements with
     Pre => M.Has_Others;
   --  Retrieve the default value for this map

   function Has_Others (M : Map) return Boolean;
   --  Say if a default has been set for this map

   procedure Insert (M : in out Map; V : String; E : Elements) with
     Post => M.Element (V) = E;
   --  Store the mapping V -> E in M. Will fail if the value is already stored.
   --  If V = "..." or "others", M.Set_Others is called internally.

   function Size (M     : Map;
                  Count : access function (E : Elements) return Natural)
                  return Natural;
   --  Count how many elements are in the map, with custom Count function (as
   --  elements may require recursive counting).

   procedure Set_Others (M : in out Map; E : Elements) with
     Post => M.Other = E;
   --  Set the default mapping for this map

private

   package Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (String, Elements);

   type Map is tagged record
      Valid   : Boolean := False;
      Base    : Variable;
      Entries : Maps.Map;
      Other   : Maps.Map; -- At most one element, key irrelevant
   end record;

   ----------
   -- Base --
   ----------

   function Base (M : Map) return Variable
   is (if M.Valid
       then M.Base
       else raise Checked_Error with "Map is uninitialized");

   -----------
   -- Empty --
   -----------

   function Empty (V : Variable) return Map
   is (Valid   => True,
       Base    => V,
       Entries => <>,
       Other   => <>);

   --------------
   -- Contains --
   --------------

   function Contains (M : Map; V : String) return Boolean
   is (M.Entries.Contains (V));

   -------------
   -- Element --
   -------------

   function Element (M : Map; V : String) return Elements
   is (if M.Contains (V)
       then M.Entries (V)
       else Other (M));

   -----------
   -- Other --
   -----------

   function Other (M : Map) return Elements
   is (if M.Other.Is_Empty
       then raise Checked_Error with
         Errors.Set ("default value in case not set")
       else M.Other.First_Element);

   ----------------
   -- Has_Others --
   ----------------

   function Has_Others (M : Map) return Boolean
   is (not M.Other.Is_Empty);

end Alire.Expressions.Maps;
