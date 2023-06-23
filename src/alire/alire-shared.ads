with Alire.Errors;
with Alire.Milestones;
with Alire.Releases.Containers;

with CLIC.User_Input;

package Alire.Shared is

   --  Stuff about shared/binary crates that are deployed not in the local
   --  workspace but in the shared configuration folder.

   function Available (Detect_Externals : Boolean := True)
                       return Releases.Containers.Release_Set;
   --  Returns the releases installed at the shared location

   function Release (Target : Milestones.Milestone;
                     Detect_Externals : Boolean := True)
                     return Releases.Release;
   --  Retrieve the release corresponding to Target, if it exists. Will raise
   --  Constraint_Error if not among Available.

   function Path return Any_Path;
   --  Returns the base folder in which all shared releases live:
   --  * <config>/cache/dependencies if set with --config/-c
   --  * <config>/cache/dependencies if set through ALR_CONFIG
   --  * ~/.cache/alire/dependencies by default

   procedure Set_Path (Path : Absolute_Path);
   --  Override the location of the global cache location

   procedure Share (Release  : Releases.Release;
                    Location : Any_Path := Path);
   --  Deploy a release in the specified location

   procedure Remove
     (Release : Releases.Release;
      Confirm : Boolean := not CLIC.User_Input.Not_Interactive)
     with Pre => Available.Contains (Release)
     or else raise Checked_Error with
       Errors.Set ("Requested release is not installed: "
                   & Release.Milestone.TTY_Image);
   --  Remove a release from the shared location for the configuration

   procedure Remove
     (Target : Milestones.Milestone;
      Confirm : Boolean := not CLIC.User_Input.Not_Interactive);
   --  Behaves as the previous Remove

   ----

   --  The following do not act on releases, but give hints to the solver.
   --  These implement the `alr with --shared` feature.

   type Requests is
     (
      No_Local,     -- The crate must not be shared (workspace)
      No_Global,    -- The crate must not be shared (global config)
      Yes_Local,    -- The crate must be shared (workspace)
      Yes_Global,   -- The crate must be shared (global config)
      Reset_Local,  -- Remove any sharing hints (workspace)
      Reset_Global, -- Remove any sharing hints (workspace)
      Default       -- Use existing sharing hints (local or global)
     );

   subtype Explicit_Requests
     is Requests range Requests'First .. Requests'Pred (Default);

   function Image (Request : Explicit_Requests) return String
   is (case Request is
          when No_Local     => "not shared in this workspace",
          when No_Global    => "not shared in global configuration",
          when Yes_Local    => "shared in this workspace",
          when Yes_Global   => "shared in global configuration",
          when Reset_Local  => "using default storage in this workspace",
          when Reset_Global => "using default storage in global configuration"
      );

   procedure Mark (Crate  : Crate_Name;
                   Status : Explicit_Requests);
   --  Store in the Level configuration that we want this dep to be reused from
   --  the shared cache, or prevent its sharing according to `Set_To`. Takes
   --  care of maintaining consistency across levels. When `Reset`, remove all
   --  hinting (both local and global) rather than setting a hint.

   type Hint is
     (
      No,     -- The crate must not be shared
      Yes,    -- The crate should be shared if possible
      Default -- Use the current default storage
     );

   function Marked_As (Crate : Crate_Name) return Hint;
   --  Says what's the hinting for the given crate. Requires being inside a
   --  root or else it will raise.

end Alire.Shared;
