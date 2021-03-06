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

--     type Value is tagged private;
   --  A particular value belonging to a Variable. See child package Enums to
--     --  create new Values.
--
--     function Base (This : Value) return Variable'Class;
--     --  Retrieve the Variable this value belongs to
--
--     function Image (This : Value) return String;
--     --  Retrieve the value representation
--
private

   --  Internally, the Variable is registered in a private storage, whereas
   --  the Variable type simply stores the key to access its declared values.
   --  This way it isn't onerous to store instances in other types. Notably,
   --  this makes the whole thing thread-unsafe.

   type Variable is tagged record
      Name : UString;
   end record;

   function From (Name : String) return Variable is (Name => +Name);

--
--     type Value is tagged record
--        Image : UString;
--     end record;
--
   function Is_Valid (This : Variable; Value : String) return Boolean
   is (False);
--
   function Name (This : Variable) return String
   is (+This.Name);
--
--     function Base (This : Value) return Variable'Class
--     is (raise Unimplemented);
--
--     function Image (This : Value) return String is (raise Unimplemented);

end Alire.Expressions;
