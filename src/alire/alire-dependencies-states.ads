private with AAA.Containers.Indefinite_Holders;

with Alire.Releases.Containers;
with Alire.TOML_Adapters;
with Alire.User_Pins;

package Alire.Dependencies.States is

   --  This type is used to store the state of a dependency post-solving. This
   --  extra information goes into the lockfile and allows tracking the status
   --  of special dependencies (pins, links, missing) across solution changes.

   type Fulfillments is (Missed,  -- Version not found, nor external definition
                         Hinted,  -- Undetected external
                         Linked,  -- Supplied for any version by a local dir
                         Solved); -- Solved with an index release/detected hint

   type Missed_Reasons is (Skipped,      -- Left out on purpose during solving.
                           --  Also when only forbidden crates fulfil some dep.
                           --  Not trivial to distinguish both with the current
                           --  solver so no explicit Forbidden reason for now.
                           Conflict,     -- Conflicting dependents
                           Unknown,      -- Crate isn't in any index
                           Unavailable); -- No version fulfils the dependency

   type Transitivities is (Unknown,   -- Needed by limitations in the solver
                           Direct,    -- A dependency of the root release
                           Indirect); -- A dependency introduced transitively

   subtype Softlink is User_Pins.Pin
     with Dynamic_Predicate =>
       Softlink.Kind in User_Pins.Kinds_With_Path;

   type State (<>) is new Dependency with private;

   ------------------
   -- Constructors --
   ------------------

   function New_State (Base : Dependency) return State;
   --  Initializes a new Missing, Unknown, Unpinned state.

   function Hinting (Base : State) return State;
   --  Change fulfilment to Hinted in copy of Base

   function Linking (Base : State;
                     Link : Softlink)
                     return State;
   --  Returns a copy of Base fulfilled by Path

   function Merging (Base     : State;
                     Versions : Semantic_Versioning.Extended.Version_Set)
                     return State;
   --  Returns a copy of Base with additional anded versions

   function Missing (Base   : State;
                     Reason : Missed_Reasons) return State;
   --  Change fulfilment to Missed in copy of Base

   function Pinning (Base : State;
                     Version : Semantic_Versioning.Version)
                     return State;
   --  Sets the pin in a copy of Base

   function Setting (Base         : State;
                     Transitivity : Transitivities)
                     return State;
   --  Modify transitivity in a copy of Base

   function Solving (Base   : State;
                     Using  : Releases.Release;
                     Shared : Boolean := False)
                     return State
     with Pre => Using.Provides (Base.Crate);
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

   function Is_Provided (This : State) return Boolean;
   --  True when the release name is different from the dependency crate

   function Is_Shared (This : State) return Boolean;

   function Is_User_Pinned (This : State) return Boolean;
   --  From the POV of users, pinning to version or linking to dir is a pin

   function Is_Solved (This : State) return Boolean;

   --  Case-specific info

   function Fulfilment (This : State) return Fulfillments;

   function Reason (This : State) return Missed_Reasons
     with Pre => This.Fulfilment = Missed;

   function Link (This : State) return Softlink
     with Pre => This.Is_Linked;

   function Pin_Version (This : State) return Semantic_Versioning.Version
     with Pre => This.Is_Pinned;

   function User_Pin (This : State) return User_Pins.Pin
     with Pre => This.Is_User_Pinned;

   function Release (This : State) return Releases.Release
     with Pre => This.Has_Release;

   function Transitivity (This : State) return Transitivities;

   --  Imaging

   overriding function Image (This : State) return String;

   function Milestone_Image (This  : State;
                             Color : Boolean := True)
                             return String
     with Pre => This.Has_Release;
   --  Will use the dep name if it differs from the dependency (due to
   --  equivalences).

   overriding function TTY_Image (This : State) return String;

   -------------------
   -- Serialization --
   -------------------

   function From_TOML (From : TOML_Adapters.Key_Queue) return State;

   overriding function To_TOML (This : State) return TOML.TOML_Value;

private

   function L (S : String) return String renames AAA.Strings.To_Lower_Case;

   use type Semantic_Versioning.Extended.Version_Set;

   type Stored_Release is new Releases.Containers.Release_H with null record;
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

   overriding
   function New_Dependency (Milestone : Milestones.Milestone;
                            Updatable : Boolean := False)
                            return State;
   --  Create a dependency from a milestone. If Updatable, use the appropriate
   --  caret/tilde versions set modifier; otherwise depend on the exact
   --  milestone.

   --  Helper types

   overriding
   function New_Dependency
     (Crate    : Crate_Name;
      Versions : Semantic_Versioning.Extended.Version_Set)
      return State;

   package Link_Holders is
     new AAA.Containers.Indefinite_Holders (Softlink);

   type Link_Holder is new Link_Holders.Holder with null record;

   function Get (This : Link_Holder) return Softlink renames Element;

   type Fulfillment_Data (Fulfillment : Fulfillments := Missed) is record
      case Fulfillment is
         when Linked =>
            Target  : Link_Holder;
            Opt_Rel : Stored_Release; -- This might not be filled-in
         when Solved =>
            Release : Stored_Release; -- This is always valid
            Shared  : Boolean;        -- The release is from shared install
         when Missed =>
            Reason  : Missed_Reasons := Skipped; -- Until solving is attempted
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
       & AAA.Strings.To_Lower_Case
         (if This.Transitivity /= Unknown
          then This.Transitivity'Img & ","
          else "")
       & AAA.Strings.To_Lower_Case (This.Fulfilled.Fulfillment'Img)
       & (if This.Fulfilled.Fulfillment = Missed
          then ":" & L (This.Fulfilled.Reason'Image)
          else "")
       & (if This.Fulfilled.Fulfillment = Linked
          then "," & This.Fulfilled.Target.Element.Image (User => True)
                   & (if not This.Fulfilled.Target.Element.Is_Broken
                      then ""
                      else ",broken")
                   & (if This.Fulfilled.Target.Element.Is_Remote
                      then ",url=" & This.Fulfilled.Target.Element.URL
                      else "")
                   & (if This.Has_Release
                      then ",release"
                   else "")
                   & (if This.Is_Shared
                      then ",installed"
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

   function Is_Provided (This : State) return Boolean
   is (This.Has_Release and then This.Release.Name /= This.Crate);

   function Is_Shared (This : State) return Boolean
   is (This.Fulfilled.Fulfillment = Solved and then This.Fulfilled.Shared);

   function Is_Solved (This : State) return Boolean
   is (This.Fulfilled.Fulfillment = Solved);

   function Is_User_Pinned (This : State) return Boolean
   is (This.Is_Pinned or else This.Is_Linked);

   ----------
   -- Link --
   ----------

   function Link (This : State) return Softlink
   is (This.Fulfilled.Target.Get);

   -------------
   -- Linking --
   -------------

   function Linking (Base : State;
                     Link : Softlink)
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

   ---------------------
   -- Milestone_Image --
   ---------------------

   function Milestone_Image (This  : State;
                             Color : Boolean := True)
                             return String
   is (if Color then
          Utils.TTY.Name (This.Crate)
          & "="
          & TTY.Version (This.Release.Version.Image)
          & (if This.Crate /= This.Release.Name
            then " (" & CLIC.TTY.Italic (This.Release.Name.As_String) & ")"
            else "")
       else
         (+This.Crate) & "=" & This.Release.Version.Image
         & (if This.Crate /= This.Release.Name
            then " (" & This.Release.Name.As_String & ")"
            else "")
      );

   -------------
   -- Missing --
   -------------

   function Missing (Base   : State;
                     Reason : Missed_Reasons) return State
   is (Base.As_Dependency with
       Name_Len     => Base.Name_Len,
       Fulfilled    => (Fulfillment => Missed,
                        Reason      => Reason),
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

   --------------------
   -- New_Dependency --
   --------------------

   overriding
   function New_Dependency (Milestone : Milestones.Milestone;
                            Updatable : Boolean := False)
                            return State
   is (New_State
       (if Updatable then
           Dependencies.New_Dependency
             (Milestone.Crate,
              Semantic_Versioning.Updatable (Milestone.Version))
        else
           Dependencies.New_Dependency
             (Milestone.Crate, Milestone.Version)));

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

   ------------
   -- Reason --
   ------------

   function Reason (This : State) return Missed_Reasons
   is (This.Fulfilled.Reason);

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

   function Solving (Base   : State;
                     Using  : Releases.Release;
                     Shared : Boolean := False)
                     return State
   is (Base.As_Dependency with
       Name_Len     => Base.Name_Len,
       Fulfilled    => (Fulfillment => Solved,
                        Release     => To_Holder (Using),
                        Shared      => Shared),
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
       & AAA.Strings.To_Lower_Case
         (if This.Transitivity /= Unknown
          then This.Transitivity'Img & ","
          else "")
       & AAA.Strings.To_Lower_Case
         (case This.Fulfilled.Fulfillment is
             when Missed =>
                TTY.Error (This.Fulfilled.Fulfillment'Img) & ":"
                & TTY.Warn (L (This.Fulfilled.Reason'Image)),
             when Hinted => TTY.Warn (This.Fulfilled.Fulfillment'Img),
             when others => This.Fulfilled.Fulfillment'Img)
       & (if This.Fulfilled.Fulfillment = Linked
          then "," & This.Fulfilled.Target.Element.Image (User => True)
                   & (if not This.Fulfilled.Target.Element.Is_Broken
                      then ""
                      else "," & TTY.Error ("broken"))
                   & (if This.Has_Release
                      then "," & TTY.OK ("release")
                      else "")
                   & (if This.Is_Shared
                      then "," & TTY.Emph ("installed")
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
       Fulfilled    => (Fulfillment => Missed,
                        Reason      => Skipped),
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

   --------------
   -- User_Pin --
   --------------

   function User_Pin (This : State) return User_Pins.Pin
   is (if This.Is_Linked
       then This.Link
       else User_Pins.New_Version (This.Pin_Version));

end Alire.Dependencies.States;
