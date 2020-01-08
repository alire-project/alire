with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Alire.Containers;
with Alire.Platforms;
with Alire.TOML_Adapters;
with Alire.Utils;

package Alire.Externals is

   --  External releases do not have an actual version until detected at
   --  runtime. Hence, they cannot be catalogued in the index with a known
   --  version. Instead, they're listed under the 'external' array.

   type External is interface;

   function Detect (This : External;
                    Name : Crate_Name) return Containers.Release_Set
                    is abstract;
   --  Perform detection and return all matching releases. Empty set must be
   --  returned if nothing can be detected. Checked_Error must be raised if
   --  detection cannot be performed normally. Caching results is allowed.
   --  Name is a convenience so Releases can be created without requiring the
   --  full containing crate reference.

   function Image (This : External) return String is abstract;
   --  Short one-liner textual description

   function Detail (This   : External;
                    Distro : Platforms.Distributions)
                    return Utils.String_Vector is abstract;
   --  Detailed longer textual description of specifics. If Distro /= Unknown,
   --  show only the relevant distro information.

   function Kind (This : External) return String is abstract;
   --  Keyword for use in `alr show` and similar displays of information

   --  Classwide helpers

   function From_TOML (From : TOML_Adapters.Key_Queue) return External'Class;

   --  Since a crate may have different externals, they'll need aggregation

   package Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (External'Class);

   type List is new Lists.List with null record;

   function Detect (This : List;
                    Name : Crate_Name) return Containers.Release_Set;

end Alire.Externals;
