private with Ada.Containers.Indefinite_Ordered_Maps;

with Alire.Errors;
private with Alire.TOML_Keys;

generic
   type Elements (<>) is private;
package Alire.Expressions.Maps with Preelaborate is

   type Map is tagged private;
   --  The main operation we need in our index expressions is to look up a
   --  value from the Variable and get the associated value (a dependency,
   --  a property...)

   function Empty (P : Variable) return Map;
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

   function Other (M : Map) return Elements with
     Pre => M.Has_Others;
   --  Retrieve the default value for this map

   function Has_Others (M : Map) return Boolean;
   --  Say if a default has been set for this map

   procedure Insert (M : in out Map; V : String; E : Elements) with
     Post => M.Element (V) = E;
   --  Store the mapping V -> E in M. Will fail if the value is already stored.
   --  If V = "...", M.Set_Others is called internally.

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

   function Empty (P : Variable) return Map
   is (Valid   => True,
       Base    => P,
       Entries => <>);

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
       else M.Other);

   -----------
   -- Other --
   -----------

   function Other (M : Map) return Elements
   is (M.Entries (TOML_Keys.Case_Others));

   ----------------
   -- Has_Others --
   ----------------

   function Has_Others (M : Map) return Boolean
   is (M.Entries.Contains (TOML_Keys.Case_Others));

end Alire.Expressions.Maps;
