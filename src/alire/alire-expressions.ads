private with Ada.Containers.Indefinite_Ordered_Maps;

with Alire.Errors;

package Alire.Expressions with Preelaborate is

   --  Support for the distinct enums/types that may appear in a case
   --  expression. This was originally done with proper Ada enumerations, for
   --  the old Ada index that exposed these types to users. Now, by using these
   --  pseudotypes we may remove all the nightmarish instantiations and have a
   --  much simpler to maintain/understand code base.

   type Variable is tagged private;
   --  A Variable is a set of values belonging to a category. E.g.,
   --  Operating_System is formed by (Linux, Windows. MacOS). Currently, only
   --  enums are supported, but to support cases on configuration variables
   --  other types will be supported in the future.

   function From (Name : String) return Variable;
   --  Retrieve a previously declared type by its name

   function Is_Valid (This : Variable; Value : String) return Boolean;
   --  Says if Value is among the values in This

   function Name (This : Variable) return String;
   --  The name that was given to a Variable (see, e.g., Enums child package)

private

   --  Internally, the Variable is registered in a private storage (see Types
   --  map below), whereas the Variable type simply stores the key to access
   --  its declared values. This way it isn't onerous to store instances in
   --  other types. Notably, this makes the whole thing thread-unsafe.

   type Variable is tagged record
      Name : UString;
   end record;

   ----------
   -- From --
   ----------

   function From (Name : String) return Variable is (Name => +Name);

   ------------
   -- Values --
   ------------

   type Values is interface;
   --  Stores the valid representations for a Variable

   function Is_Valid (V : Values; Image : String) return Boolean is abstract;
   --  Say if a value, given as its string image, matches a value of a type

   package Variable_Value_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (String, Values'Class);

   -----------
   -- Types --
   -----------

   Types : Variable_Value_Maps.Map;
   --  Stores all types that have been declared, with their values

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : Variable; Value : String) return Boolean
   is (if not Types.Contains (Name (This))
       then raise Checked_Error with
         Errors.Set ("Expression variable '" & Name (This) & "' is unknown")
       else Types (Name (This)).Is_Valid (Value));

   ----------
   -- Name --
   ----------

   function Name (This : Variable) return String
   is (+This.Name);

end Alire.Expressions;
