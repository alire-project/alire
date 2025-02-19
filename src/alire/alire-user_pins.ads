with Alire.Optional;
with Alire.TOML_Adapters;
with Alire.VCSs.Git;

with Semantic_Versioning;

with TOML;

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

   function Image (This : Pin; User : Boolean) return String;
   --  Returns the internal information as-is or with relative paths, when User

   function Is_Remote (This : Pin) return Boolean;
   --  A pin to a remote source such as git, source archives, etc

   --  Version pins

   function New_Version (Version : Semantic_Versioning.Version) return Pin
     with Post => New_Version'Result.Kind = To_Version;

   function Version (This : Pin) return Semantic_Versioning.Version
     with Pre => This.Kind = To_Version;

   --  Local path pins

   function New_Path (Path : Any_Path) return Pin
     with Post => New_Path'Result.Kind = To_Path;

   function Is_Broken (This : Pin) return Boolean
     with Pre => This.Kind in Kinds_With_Path;

   function Has_Path (This : Pin) return Boolean;
   --  True if Path will return a path for a local or deployed link, False if
   --  Path will raise.

   function Path (This : Pin) return Absolute_Path
     with Pre => This.Kind in Kinds_With_Path;
   --  May raise if a Git pin hasn't been yet deployed (see Deploy proc). Even
   --  if paths can be given as relative, for our internal processing we can
   --  simplify things by always relying on absolute paths.

   function Relative_Path (This : Pin; Color : Boolean := True) return String
     with Pre => This.Kind in Kinds_With_Path;
   --  Convenience to show to users. May still return an absolute path for
   --  paths in another drive on Windows. May include TTY sequences.

   --  Remote pins

   function New_Remote (URL    : Alire.URL;
                        Commit : String := "";
                        Branch : String := "";
                        Subdir : Alire.Relative_Path := "")
                        return Pin
     with
       Pre => Commit = "" or else VCSs.Git.Is_Valid_Commit (Commit),
       Post => New_Remote'Result.Kind = To_Git;

   function URL (This : Pin) return Alire.URL
     with Pre => This.Is_Remote;

   function Branch (This : Pin) return Optional.String
     with Pre => This.Is_Remote;

   function Commit (This : Pin) return Optional.String
     with Pre => This.Is_Remote;

   function Subdir (This : Pin) return Optional.String with
     Pre => This.Is_Remote,
     Post =>
       (if Subdir'Result.Has_Element
        then not Check_Absolute_Path (Subdir'Result.Value));

   function TTY_URL_With_Reference (This     : Pin;
                                    Detailed : Boolean := False)
                                    return String
     with Pre => This.Is_Remote;
   --  returns https://blah[#commit|#branch], when existing

   function Deploy_Path (This  : Pin;
                         Crate : Crate_Name;
                         Under : Any_Path)
                         return Absolute_Path
     with Pre => This.Kind in Kinds_With_Path;
   --  Says where this pin would be deployed, without doing nothing else. Under
   --  is the umbrella folder for all pins of a root.

   procedure Deploy (This   : in out Pin;
                     Crate  : Crate_Name;
                     Under  : Any_Path;
                     Online : Boolean)
     with Pre  => This.Kind in Kinds_With_Path,
          Post => Path (This) /= "";
   --  Will fetch a remote pin and fill its local path; it is a no-op
   --  otherwise. Under is the umbrella folder for all pins, not the final pin
   --  destination. If Online, branch pins will be checked for updates. Any pin
   --  sources not at their expected final path (computed in here depending on
   --  the pin kind) will be checked out anyway.

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

   function To_TOML (This : Pin) return TOML.TOML_Value
     with Pre => This.Kind in Kinds_With_Path;
   --  Used by the lockfile

   function To_Manifest_Line (This  : Pin;
                              Crate : Crate_Name)
                              return String;
   --  Returns the single line that describes this pin in a manifest

private

   type Pin (Kind : Kinds) is tagged record
      case Kind is
         when To_Git =>
            URL           : UString;
            Branch        : UString; -- Optional
            Commit        : UString; -- Optional
            Checkout_Path : Unbounded_Absolute_Path;
            --  Empty until the repo is locally deployed
            Subdir        : Unbounded_Relative_Path;
            --  For monorepos, subdir in which the crate is found
         when To_Path =>
            Local_Path : Unbounded_Absolute_Path;
         when To_Version =>
            Version : Semantic_Versioning.Version;
      end case;
   end record;

   ------------
   -- Branch --
   ------------

   function Branch (This : Pin) return Optional.String
   is (if +This.Branch = ""
       then Optional.Strings.Empty
       else Optional.Strings.Unit (+This.Branch));

   ------------
   -- Commit --
   ------------

   function Commit (This : Pin) return Optional.String
   is (if +This.Commit = ""
       then Optional.Strings.Empty
       else Optional.Strings.Unit (+This.Commit));

   ------------
   -- Subdir --
   ------------

   function Subdir (This : Pin) return Optional.String
   is (if +This.Subdir = ""
       then Optional.Strings.Empty
       else Optional.Strings.Unit (+This.Subdir));

   ---------------
   -- Is_Remote --
   ---------------

   function Is_Remote (This : Pin) return Boolean
   is (This.Kind in To_Git);
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
