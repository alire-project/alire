with Alire.Properties;
with Alire.TOML_Keys;

package Alire.Expressions with Preelaborate is

   --  Support for the distinct enums/types that may appear in a case
   --  expression. This was originally done with proper Ada enumerations, for
   --  the old Ada index that exposed these types to users. Now, by using these
   --  pseudotypes we may remove all the nightmarish instantiations and have a
   --  much simpler to maintain/understand code base.

   type Variable is tagged private;
   --  A Variable is a set of values belonging to a category. E.g.,
   --  Operating_System is formed by (Linux, Windows. MacOS). Currently, only
   --  enums are supported, but to allow cases on configuration variables
   --  other types may be added in the future.

   function From (Key : String) return Variable;
   --  Retrieve a previously declared type by its TOML key

   function Is_Valid (This : Variable; Value : String) return Boolean;
   --  Says if Value is among the values in This

   function Key (This : Variable) return String;
   --  The key that is used in TOML files for this variable

   function Name (This : Variable) return String;
   --  The Ada-like name of this variable

   function Satisfies (Property : Properties.Property'Class;
                       Var_Key  : String;
                       Value    : String) return Boolean
     with Pre => Value /= TOML_Keys.Case_Others and then
                 Value /= "others";
   --  Say if a property is satisfied by the value, which must match the
   --  Variable and property type (keys must match). Doesn't accept defaults.

private

   --  Internally, the Variable is registered in a private storage (see
   --  Variables map in body), whereas the Variable type simply stores the
   --  key to access its declared values. This way it isn't onerous to store
   --  instances in every case node in the index. Incidentally, this makes the
   --  whole thing thread-unsafe.

   type Variable is tagged record
      Key  : UString;
      Name : UString;
   end record;

   ------------
   -- Values --
   ------------

   type Values is interface;
   --  Stores the valid representations for a Variable

   function Is_Valid (V : Values; Image : String) return Boolean is abstract;
   --  Say if a value, given as its string image, matches a value of a type

   procedure Register (Var_Key    : String;
                       Var_Name   : String;
                       Var_Values : Values'Class);
   --  Makes Alire aware of the existence of a variable usable in expressions

   ---------
   -- Key --
   ---------

   function Key (This : Variable) return String
   is (+This.Key);

   ----------
   -- Name --
   ----------

   function Name (This : Variable) return String
   is (+This.Name);

end Alire.Expressions;
