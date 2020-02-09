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

end Alire.Externals.Lists;
