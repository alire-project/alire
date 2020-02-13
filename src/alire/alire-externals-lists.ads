with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Alire.Properties;

package Alire.Externals.Lists is

   --  Since a crate may have different externals, they'll need aggregation

   package Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (External'Class);

   type List is new Lists.List with null record;

   function Detect (This : List;
                    Name : Crate_Name;
                    Env  : Properties.Vector) return Containers.Release_Set;
   --  Goes over the externals defined in List and, when Available, performs
   --  their Detect call.

   function Hints (This : List;
                   Name : Crate_Name;
                   Env  : Properties.Vector := Properties.No_Properties)
                   return Utils.String_Vector;
   --  Given an external list, evaluate all that apply under Env platform
   --  properties and, for those that fail to detect, return their hint
   --  message if any. If Env is empty, return all hints unconditionally.

end Alire.Externals.Lists;
