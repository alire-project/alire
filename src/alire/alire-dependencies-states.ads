private with AAA.Containers.Indefinite_Holders;

private with Alire.Containers;
with Alire.Externals.Softlinks;
with Alire.Releases;
with Alire.TOML_Adapters;

package Alire.Dependencies.States is

   --  This type is used to store the state of a dependency post-solving. This
   --  extra information goes into the lockfile and allows tracking the status
   --  of special dependencies (pins, links, missing) across solution changes.

   type Fulfillments is (Missed,  -- Version not found, nor external definition
                         Hinted,  -- Undetected external
                         Linked,  -- Supplied for any version by a local dir
                         Solved); -- Solved with an index release/detected hint

   type Transitivities is (Unknown,   -- Needed by limitations in the solver
                           Direct,    -- A dependency of the root release
                           Indirect); -- A dependency introduced transitively

   type State (<>) is new Dependency with private;

   ------------------
   -- Constructors --
   ------------------

   function New_State (Base : Dependency) return State;
   --  Initializes a new Missing, Unknown, Unpinned state.

   function Hinting (Base : State) return State;
   --  Change fulfilment to Hinted in copy of Base

   function Linking (Base : State;
                     Link : Externals.Softlinks.External)
                     return State;
   --  Returns a copy of Base fulfilled by Path

   function Merging (Base     : State;
                     Versions : Semantic_Versioning.Extended.Version_Set)
                     return State;
   --  Returns a copy of Base with additional anded versions

   function Missing (Base : State) return State;
   --  Change fulfilment to Missed in copy of Base

   function Pinning (Base : State;
                     Version : Semantic_Versioning.Version)
                     return State;
   --  Sets the pin in a copy of Base

   function Setting (Base         : State;
                     Transitivity : Transitivities)
                     return State;
   --  Modify transitivity in a copy of Base

   function Solving (Base  : State;
                     Using : Releases.Release)
                     return State
     with Pre => Base.Crate = Using.Name;
   --  Uses release to fulfill this dependency in a copy of Base

   function Unlinking (Base : State) return State;
   --  Unlinks the crate in a copy of Base, becoming Missed

   function Unpinning (Base : State) return State;
   --  Removes the pin in a copy of Base

   ----------------
   -- Attributes --
   ----------------

   function As_Dependency (This : State) return Dependencies.Dependency;
   --  Upcast for convenience, equivalent to Dependencies.Dependency (This)

   function Has_Release (This : State) return Boolean;
   --  Says if a release can be retrieved for this state (a solved dependency,
   --  or a linked crate folder with alire context).

   --  Simple status identification

   function Is_Direct (This : State) return Boolean;

   function Is_Hinted (This : State) return Boolean;

   function Is_Indirect (This : State) return Boolean;

   function Is_Linked (This : State) return Boolean;

   function Is_Missing (This : State) return Boolean;

   function Is_Pinned (This : State) return Boolean;

   function Is_User_Pinned (This : State) return Boolean;
   --  From the POV of users, pinning to version or linking to dir is a pin

   function Is_Solved (This : State) return Boolean;

   --  Case-specific info

   function Fulfilment (This : State) return Fulfillments;

   function Link (This : State) return Externals.Softlinks.External
     with Pre => This.Is_Linked;

   function Pin_Version (This : State) return Semantic_Versioning.Version
     with Pre => This.Is_Pinned;

   function Release (This : State) return Releases.Release
     with Pre => This.Has_Release;

   function Transitivity (This : State) return Transitivities;

   --  Imaging

   overriding function Image (This : State) return String;

   overriding function TTY_Image (This : State) return String;

   -------------------
   -- Serialization --
   -------------------

   function From_TOML (From : TOML_Adapters.Key_Queue) return State;

   overriding function To_TOML (This : State) return TOML.TOML_Value;

private

   use type Semantic_Versioning.Extended.Version_Set;

   type Stored_Release is new Containers.Release_H with null record;
   --  New type to simplify comparison of optional stored releases

   overriding function "=" (L, R : Stored_Release) return Boolean;
   --  Comparing releases directly returns always false due to some internal
   --  discrepancy not yet clear. For the purposes of comparing solutions, we
   --  rely on the milestone and origin for solved releases, which is the kind
   --  of uniqueness we want at this level.

   --  Helper functions

   function Optional_Release (Crate     : Crate_Name;
                              Workspace : Any_Path)
                              return Stored_Release;
   --  Detect if Workspace is a valid Alire crate for the given Crate, in which
   --  case the returned release will be valid. Otherwise it will be empty. If
   --  the crate found does not match Crate in name, a Checked_Error will be
   --  raised.

   --  Base overridings

   overriding
   function From_String (Unused_Spec : String) return State
   is (raise Program_Error with "Not intended for use");

   overriding
   function From_TOML (Unused_Key   : String;
                       Unused_Value : TOML.TOML_Value) return State;

   overriding
   function New_Dependency (Crate   : Crate_Name;
                            Version : Semantic_Versioning.Version)
                            return State;

   --  Helper types

   package External_Holders is
     new AAA.Containers.Indefinite_Holders (Externals.Softlinks.External);

   type Link_Holder is new External_Holders.Holder with null record;

   overriding
   function New_Dependency
     (Crate    : Crate_Name;
      Versions : Semantic_Versioning.Extended.Version_Set)
      return State;

   type Fulfillment_Data (Fulfillment : Fulfillments := Missed) is record
      case Fulfillment is
         when Linked =>
            Target  : Link_Holder;
            Opt_Rel : Stored_Release; -- This might not be filled-in
         when Solved =>
            Release : Stored_Release; -- This is always valid
         when others => null;
      end case;
   end record;

   type Pinning_Data (Pinned : Boolean := False) is record
      case Pinned is
         when True  => Version : Semantic_Versioning.Version;
         when False => null;
      end case;
   end record;

   -----------
   -- State --
   -----------

   type State (Name_Len : Natural) is new Dependency (Name_Len) with record
      Fulfilled    : Fulfillment_Data;
      Pinning      : Pinning_Data;
      Transitivity : Transitivities := Unknown;
   end record;

   -------------------
   -- As_Dependency --
   -------------------

   function As_Dependency (This : State) return Dependencies.Dependency
   is (Dependencies.Dependency (This));

   ---------------
   -- From_TOML --
   ---------------

   overriding
   function From_TOML (Unused_Key   : String;
                       Unused_Value : TOML.TOML_Value) return State
   is (raise Unimplemented); -- not needed

   ----------------
   -- Fulfilment --
   ----------------

   function Fulfilment (This : State) return Fulfillments
   is (This.Fulfilled.Fulfillment);

   -----------------
   -- Has_Release --
   -----------------

   function Has_Release (This : State) return Boolean
   is (This.Is_Solved or else
         (This.Is_Linked and then not This.Fulfilled.Opt_Rel.Is_Empty));

   -------------
   -- Hinting --
   -------------

   function Hinting (Base : State) return State
   is (Base.As_Dependency with
       Name_Len     => Base.Name_Len,
       Fulfilled    => (Fulfillment => Hinted),
       Pinning      => Base.Pinning,
       Transitivity => Base.Transitivity);

   -----------
   -- Image --
   -----------

   overriding function Image (This : State) return String
   is (This.As_Dependency.Image
       & " ("
       & Utils.To_Lower_Case
         (if This.Transitivity /= Unknown
          then This.Transitivity'Img & ","
          else "")
       & Utils.To_Lower_Case (This.Fulfilled.Fulfillment'Img)
       & (if This.Fulfilled.Fulfillment = Linked
          then ",pin=" & This.Fulfilled.Target.Get.Path
                       & (if GNAT.OS_Lib.Is_Directory
                            (This.Fulfilled.Target.Get.Path)
                             then ""
                             else "," & TTY.Error ("broken"))
                       & (if This.Has_Release
                          then ",release"
                          else "")
          else "")
       & (if This.Pinning.Pinned
          then ",pin=" & This.Pinning.Version.Image
          else "")
       & ")");

   --------
   -- Is --
   --------

   function Is_Direct (This : State) return Boolean
   is (This.Transitivity = Direct);

   function Is_Hinted (This : State) return Boolean
   is (This.Fulfilled.Fulfillment = Hinted);

   function Is_Indirect (This : State) return Boolean
   is (This.Transitivity = Indirect);

   function Is_Linked (This : State) return Boolean
   is (This.Fulfilled.Fulfillment = Linked);

   function Is_Missing (This : State) return Boolean
   is (This.Fulfilled.Fulfillment = Missed);

   function Is_Pinned (This : State) return Boolean
   is (This.Pinning.Pinned);

   function Is_Solved (This : State) return Boolean
   is (This.Fulfilled.Fulfillment = Solved);

   function Is_User_Pinned (This : State) return Boolean
   is (This.Is_Pinned or else This.Is_Linked);

   ----------
   -- Link --
   ----------

   function Link (This : State) return Externals.Softlinks.External
   is (This.Fulfilled.Target.Get);

   -------------
   -- Linking --
   -------------

   function Linking (Base : State;
                     Link : Externals.Softlinks.External)
                     return State
   is (Base.As_Dependency with
       Name_Len     => Base.Name_Len,
       Fulfilled    => (Fulfillment => Linked,
                        Target      => To_Holder (Link),
                        Opt_Rel     => Optional_Release (Base.Crate,
                                                         Link.Path)),
       Pinning      => Base.Pinning,
       Transitivity => Base.Transitivity);

   -------------
   -- Merging --
   -------------

   function Merging (Base     : State;
                     Versions : Semantic_Versioning.Extended.Version_Set)
                     return State
   is (Dependencies.New_Dependency (Base.Crate,
                                    Base.Versions and Versions) with
       Name_Len     => Base.Name_Len,
       Fulfilled    => Base.Fulfilled,
       Pinning      => Base.Pinning,
       Transitivity => Base.Transitivity);

   -------------
   -- Missing --
   -------------

   function Missing (Base : State) return State
   is (Base.As_Dependency with
       Name_Len     => Base.Name_Len,
       Fulfilled    => (Fulfillment => Missed),
       Pinning      => Base.Pinning,
       Transitivity => Base.Transitivity);

   --------------------
   -- New_Dependency --
   --------------------

   overriding
   function New_Dependency (Crate   : Crate_Name;
                            Version : Semantic_Versioning.Version)
                            return State
   is (New_State (Dependencies.New_Dependency (Crate, Version)));

   --------------------
   -- New_Dependency --
   --------------------

   overriding
   function New_Dependency
     (Crate    : Crate_Name;
      Versions : Semantic_Versioning.Extended.Version_Set)
      return State
   is (New_State (Dependencies.New_Dependency (Crate, Versions)));

   ---------------
   -- New_State --
   ---------------

   function New_State (Base : Dependency) return State
   is (State'(Base with
              Name_Len => Base.Crate.Name'Length,
              others   => <>));

   -----------------
   -- Pin_Version --
   -----------------

   function Pin_Version (This : State) return Semantic_Versioning.Version
   is (This.Pinning.Version);

   -------------
   -- Pinning --
   -------------

   function Pinning (Base    : State;
                     Version : Semantic_Versioning.Version)
                     return State
   is (Base.As_Dependency with
       Name_Len     => Base.Name_Len,
       Fulfilled    => Base.Fulfilled,
       Pinning      => (Pinned  => True,
                        Version => Version),
       Transitivity => Base.Transitivity);

   -------------
   -- Release --
   -------------

   function Release (This : State) return Releases.Release
   is (if This.Is_Solved then
          This.Fulfilled.Release.Element
       elsif This.Is_Linked then
          This.Fulfilled.Opt_Rel.Element
       else raise Program_Error
         with "dependency has no release: " & This.Crate.TTY_Image);

   -------------
   -- Setting --
   -------------

   function Setting (Base         : State;
                     Transitivity : Transitivities)
                     return State
   is (Base.As_Dependency with
       Name_Len     => Base.Name_Len,
       Fulfilled    => Base.Fulfilled,
       Pinning      => Base.Pinning,
       Transitivity => Transitivity);

   -------------
   -- Solving --
   -------------

   function Solving (Base  : State;
                     Using : Releases.Release)
                     return State
   is (Base.As_Dependency with
       Name_Len     => Base.Name_Len,
       Fulfilled    => (Fulfillment => Solved,
                        Release     => To_Holder (Using)),
       Pinning      => Base.Pinning,
       Transitivity => Base.Transitivity);

   ------------------
   -- Transitivity --
   ------------------

   function Transitivity (This : State) return Transitivities
   is (This.Transitivity);

   ---------------
   -- TTY_Image --
   ---------------

   overriding function TTY_Image (This : State) return String
   is (This.As_Dependency.TTY_Image
       & " ("
       & Utils.To_Lower_Case
         (if This.Transitivity /= Unknown
          then This.Transitivity'Img & ","
          else "")
       & Utils.To_Lower_Case
         (case This.Fulfilled.Fulfillment is
             when Missed => TTY.Error (This.Fulfilled.Fulfillment'Img),
             when Hinted => TTY.Warn (This.Fulfilled.Fulfillment'Img),
             when others => This.Fulfilled.Fulfillment'Img)
       & (if This.Fulfilled.Fulfillment = Linked
          then "," & TTY.Emph ("pin") & "="
                   & TTY.URL (This.Fulfilled.Target.Get.Path)
                   & (if GNAT.OS_Lib.Is_Directory
                        (This.Fulfilled.Target.Get.Path)
                         then ""
                         else "," & TTY.Error ("broken"))
                   & (if This.Has_Release
                      then "," & TTY.OK ("release")
                      else "")
          else "")
       & (if This.Pinning.Pinned
          then "," & TTY.Emph ("pin")
                   & "=" & TTY.Version (This.Pinning.Version.Image)
          else "")
       & ")");

   ---------------
   -- Unlinking --
   ---------------

   function Unlinking (Base : State) return State
   is (Base.As_Dependency with
       Name_Len     => Base.Name_Len,
       Fulfilled    => (Fulfillment => Missed),
       Pinning      => Base.Pinning,
       Transitivity => Base.Transitivity);

   ---------------
   -- Unpinning --
   ---------------

   function Unpinning (Base : State) return State
   is (Base.As_Dependency with
       Name_Len     => Base.Name_Len,
       Fulfilled    => Base.Fulfilled,
       Pinning      => (Pinned => False),
       Transitivity => Base.Transitivity);

end Alire.Dependencies.States;
