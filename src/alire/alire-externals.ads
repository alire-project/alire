with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Alire.Containers;

package Alire.Externals with Preelaborate is

   --  External releases do not have an actual version until detected at
   --  runtime. Hence, they cannot be catalogued in the index with a known
   --  version. Instead, they're listed under the 'external' array.

   type External is interface;

   function Detect (This : External) return Containers.Release_Set
                    is abstract;
   --  Perform detection and return all matching releases. Empty set must be
   --  returned if nothing can be detected. Checked_Error must be raised if
   --  detection cannot be performed normally. Caching results is allowed.

   function Image (This : External) return String is abstract;
   --  Short textual description

   package Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (External'Class);

   type List is new Lists.List with null record;

   function Detect (This : List) return Containers.Release_Set;

end Alire.Externals;
