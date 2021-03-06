generic
   Name : String with Unreferenced;
   type Values is (<>) with Unreferenced;
package Alire.Expressions.Enums is

   --  Declare a pseudotype that encapsulates an actual Ada enumeration. By
   --  instancing this package the type becomes recognizable by the index
   --  loading functions.

--
--     function The_Type return Pseudotype;
--     --  Retrieve the type identity
--
--     function Value (V : Values) return Pseudotypes.Value;
--     --  Wrapped representation of the actual enum
--
--  private
--
--     function The_Type return Pseudotype is (raise Unimplemented);
--
--     function Value (V : Values) return Pseudotypes.Value
--     is (raise Unimplemented);

end Alire.Expressions.Enums;
