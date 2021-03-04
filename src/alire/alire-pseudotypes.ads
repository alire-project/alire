private package Alire.Pseudotypes with Preelaborate is

   --  Support for the distinct enums/types that may appear in a case
   --  expression. This was originally done with proper Ada enumerations, for
   --  the old Ada index that exposed these types to users. Now, by using these
   --  pseudotypes we may remove all the nightmarish instantiations and have a
   --  much simpler to maintain/understand code base.

   type Pseudotype is tagged private;
   --  A pseudotype is a set of values belonging to a category. E.g.,
   --  Operating_System is formed by (Linux, Windows. MacOS). Currently, only
   --  enums are supported, but to support cases on configuration variables
   --  other types will be supported in the future.

   function Is_Valid (This : Pseudotype; Value : String) return Boolean;
   --  Says if Value is among the values in This

   function Name (This : Pseudotype) return String;
   --  The name that was given to a pseudotype (see, e.g., Enums child package)

   type Value is tagged private;
   --  A particular value belonging to a pseudotype. See child package Enums to
   --  create new Values.

   function Base (This : Value) return Pseudotype'Class;
   --  Retrieve the pseudotype this value belongs to

   function Image (This : Value) return String;
   --  Retrieve the value representation

private

   --  Internally, the pseudotype is registered in a private storage, whereas
   --  the Pseudotype type simply stores the key to access its declared values.
   --  This way it isn't onerous to store instances in other types. Notably,
   --  this makes the whole thing thread-unsafe.

   type Pseudotype is tagged record
      Name : UString;
   end record;

   type Value is tagged record
      Image : UString;
   end record;

end Alire.Pseudotypes;
