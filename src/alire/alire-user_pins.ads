with Alire.Optional;
with Alire.TOML_Adapters;

with Semantic_Versioning;

package Alire.User_Pins is

   --  User-facing representation of pins. These are loaded from the manifest.
   --  Internally, a user pin can be either a pin to a version, or a softlink
   --  to a folder. Note that, as they cannot exist in the index (we would not
   --  want an indexed crate to depend on unknown folders/remotes), there is no
   --  need to generate TOML for them.

   --  The information provided by the user in the pin is not complete to work
   --  with. The root must check if a remote pin was already retrieved, and if
   --  it matches the user description, or else fetch it, when a change in the
   --  manifest is detected.

   type Kinds is (To_Git,
                  To_Path,
                  To_Version);

   subtype Kinds_With_Path is Kinds range To_Git .. To_Path;

   type Pin (Kind : Kinds) is tagged private;

   function Is_Remote (This : Pin) return Boolean;
   --  A pin to a remote source such as git, source archives, etc

   --  Version attributes

   function Version (This : Pin) return Semantic_Versioning.Version
     with Pre => This.Kind = To_Version;

   --  Local path attributes

   function Path (This : Pin) return Absolute_Path
     with Pre => This.Kind in Kinds_With_Path;
   --  May raise if a Git pin hasn't been yet deployed (see Deploy proc). Even
   --  if paths can be given as relative, for our internal processing we can
   --  simplify things by always relying on absolute paths. As these paths are
   --  not re-written to disk, there is no harm in doing this. Also, showing
   --  absolute paths to the user is less confusing that relative paths from
   --  unclear locations.

   function Relative_Path (This : Pin) return Any_Path
     with Pre => This.Kind in Kinds_With_Path;
   --  Convenience to compare lockfile paths to pin paths. May still return an
   --  absolute path for paths in another drive on windows.

   --  Remote attributes

   function URL (This : Pin) return Alire.URL
     with Pre => This.Is_Remote;

   function Commit (This : Pin) return Optional.String
     with Pre => This.Is_Remote;

   procedure Deploy (This   : in out Pin;
                     Under  : Any_Path;
                     Online : Boolean)
     with Post => (if This.Kind in Kinds_With_Path then This.Path /= "");
   --  Will fetch a remote pin and fill its local path; it is a no-op
   --  otherwise. Under is the umbrella folder for all pins, not the final pin
   --  destination. If Online, branch pins will be checked for updates. Any pin
   --  not at their expected final path (computed in here depending on the pin
   --  kind) will be checked out anyway.

   --  Pin loading from manifest

   function From_TOML (This : TOML_Adapters.Key_Queue) return Pin;
   --  Expects the rhs of a crate = <pin> entry. The rhs is always a table.
   --  Must be called with PWD being the same as of the manifest that is being
   --  loaded, so relative pins are correct.

   --  The TOML representation of a pin is similar to a dependency, but instead
   --  of a version set, we get either a precise version, or an url + commit:
   --  [[pins]]
   --  foo = "3.4" -- OR:
   --  foo = { version = "5.6" }
   --  foo = { path = "/path/to/folder" }
   --  bar = { url = "git+https://blah", [commit = "deadbeef"] }

private

   type Pin (Kind : Kinds) is tagged record
      case Kind is
         when To_Git =>
            URL        : UString;
            Commit     : UString; -- Optional
            Local_Path : UString; -- Empty until the pin is locally deployed
         when To_Path =>
            Path : UString;
         when To_Version =>
            Version : Semantic_Versioning.Version;
      end case;
   end record;

   ------------
   -- Commit --
   ------------

   function Commit (This : Pin) return Optional.String
   is (if +This.Commit = ""
       then Optional.Strings.Empty
       else Optional.Strings.Unit (+This.Commit));

   ---------------
   -- Is_Remote --
   ---------------

   function Is_Remote (This : Pin) return Boolean
   is (This.Kind in To_Git);

   ----------
   -- Path --
   ----------

   function Path (This : Pin) return Absolute_Path
   is (case This.Kind is
          when To_Path => +This.Path,
          when To_Git  => (if +This.Local_Path /= ""
                           then +This.Local_Path
                           else raise Program_Error with "Undeployed pin"),
          when others  => raise Program_Error with "invalid pin kind");

   ---------
   -- URL --
   ---------

   function URL (This : Pin) return Alire.URL
   is (+This.URL);

   -------------
   -- Version --
   -------------

   function Version (This : Pin) return Semantic_Versioning.Version
   is (This.Version);

end Alire.User_Pins;