with Ada.Containers.Indefinite_Ordered_Maps;

with Alire.Externals.Softlinks.Holders;
with Alire.Milestones.Holders;
with Alire.TOML_Adapters;

package Alire.User_Pins is

   --  User-facing representation of pins. These are loaded from the manifest.
   --  Internally, a user pin can be either a pin to a version, or a softlink
   --  to a folder. Note that, as they cannot exist in the index (we would not
   --  want an indexed crate to depend on unknown folders/remotes), there is no
   --  need to generate TOML for them.

   type Kinds is (To_External, To_Version);

   type Pin (Kind : Kinds) is tagged record
      case Kind is
         when To_External =>
            Softlink : Externals.Softlinks.Holders.Holder;
         when To_Version =>
            Version : Milestones.Holders.Holder;
      end case;
   end record;

   function From_TOML (This : TOML_Adapters.Key_Queue) return Pin;

   --  The TOML representation of a pin is similar to a dependency, but instead
   --  of a version set, we get either a precise version, or an url + commit:
   --  [[pins]]
   --  foo = "5.6"
   --  bar = { url = "git+https://blah", commit = }

   package Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Crate_Name, Pin);

   subtype Map is Maps.Map;

end Alire.User_Pins;
