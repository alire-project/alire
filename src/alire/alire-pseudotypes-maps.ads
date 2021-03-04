with Alire.Errors;

generic
   type Elements (<>) is private;
package Alire.Pseudotypes.Maps with Preelaborate is

   type Map is tagged private;
   --  The main operation we need in our index expressions is to look up a
   --  value from the pseudotype and get the associated value (a dependency,
   --  a property...)

   function Empty (P : Pseudotype) return Map;
   --  Initialize a map for a particular type, containing no mapping

   function Contains (M : Map; V : Value) return Boolean;

   function Element (M : Map; V : Value) return Elements with
     Pre => M.Contains (V) or else M.Has_Others or else
     raise Checked_Error with
       Errors.Set ("Map for " & V.Base.Name
                   & " does not have a value for " & V.Image);
   --  Get an element from the map

   function Other (M : Map) return Elements with
     Pre => M.Has_Others;
   --  Retrieve the default value for this map

   function Has_Others (M : Map) return Boolean;
   --  Say if a default has been set for this map

   procedure Include (M : in out Map; V : Value; E : Elements) with
     Post => M.Element (V) = E;
   --  Store the mapping V -> E in M, overwritting a previous one

   procedure Set_Others (M : in out Map; E : Elements) with
     Post => M.Other = E;
   --  Set the default mapping for this map

private

   type Map is tagged null record;

   function Empty (P : Pseudotype) return Map is (raise Unimplemented);

   function Contains (M : Map; V : Value) return Boolean
   is (raise Unimplemented);

   function Element (M : Map; V : Value) return Elements
   is (raise Unimplemented);

   function Other (M : Map) return Elements is (raise Unimplemented);

   function Has_Others (M : Map) return Boolean is (raise Unimplemented);

   procedure Include (M : in out Map; V : Value; E : Elements)
   is null;

   procedure Set_Others (M : in out Map; E : Elements)
   is null;

end Alire.Pseudotypes.Maps;
