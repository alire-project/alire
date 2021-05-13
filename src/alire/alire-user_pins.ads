with Alire.TOML_Adapters;

with Semantic_Versioning;

package Alire.User_Pins is

   --  User-facing representation of pins. These are loaded from the manifest.
   --  Internally, a user pin can be either a pin to a version, or a softlink
   --  to a folder. Note that, as they cannot exist in the index (we would not
   --  want an indexed crate to depend on unknown folders/remotes), there is no
   --  need to generate TOML for them.

   --  The information stored for user pins is still incomplete. The root
   --  must check if the pin was already retrieved, and if it matches the
   --  user description, or else fetch it, when a change in the manifest
   --  is detected.

   type Kinds is (To_Git,
                  To_Path,
                  To_Version);

   type Pin (Kind : Kinds) is tagged private;

   function From_TOML (This : TOML_Adapters.Key_Queue) return Pin;
   --  Expects the rhs of a crate = <pin> entry. This can be a string (for a
   --  version) or a table (for an origin).

   --  The TOML representation of a pin is similar to a dependency, but instead
   --  of a version set, we get either a precise version, or an url + commit:
   --  [[pins]]
   --  foo = "5.6"
   --  bar = { url = "git+https://blah", commit = }

private

   type Pin (Kind : Kinds) is tagged record
      case Kind is
         when To_Git =>
            URL    : UString;
            Commit : UString; -- Optional
         when To_Path =>
            Path : UString;
         when To_Version =>
            Version : Semantic_Versioning.Version;
      end case;
   end record;

end Alire.User_Pins;
