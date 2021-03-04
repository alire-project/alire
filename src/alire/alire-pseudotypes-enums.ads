generic
   Name : String;
   type Values is (<>);
package Alire.Pseudotypes.Enums is

   --  Declare a pseudotype that encapsulates an actual Ada enumeration

   function The_Type return Pseudotype;
   --  Retrieve the type identity

   function Value (V : Values) return Pseudotypes.Value;
   --  Wrapped representation of the actual enum

end Alire.Pseudotypes.Enums;
