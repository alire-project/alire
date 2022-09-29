with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

with Alire.Containers;
with Alire.Index_On_Disk.Loading;
with Alire.Utils.TTY;

package body Alire.Index is

   package Index_Loading renames Index_On_Disk.Loading;

   package Release_Set_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Crate_Name, Releases.Containers.Release_Set,
        "<",        Releases.Containers."=");
   subtype Release_Alias_Map is Release_Set_Maps.Map;

   use all type Semantic_Versioning.Version;

   Contents : aliased Alire.Crates.Containers.Maps.Map;
   --  Regular mapping from crate name to its releases

   Release_Aliases : Release_Alias_Map;
   --  Mapping from crate name to any release that satisfies it. Currently,
   --  releases are duplicated in memory. These two collections could be made
   --  to share releases via some indirection or pointers.

   Crate_Aliases : aliased Provides.Crate_Provider_Map;
   --  During on-demand crate loading, we need to know which crates also
   --  provide the requested crate. This information is redundant with
   --  Release_Aliases, but as this simpler information is the one that's
   --  read/written to disk on index update, we better keep a separate copy
   --  for simplicity.

   ---------
   -- Add --
   ---------

   procedure Add (Crate  : Crates.Crate;
                  Policy : Policies.For_Index_Merging :=
                    Policies.Merge_Priorizing_Existing) is
   begin
      if Contents.Contains (Crate.Name) then
         declare
            Old : Crates.Crate := Contents (Crate.Name);
         begin
            case Policy is
               when Policies.Merge_Priorizing_Existing =>
                  for Release of Crate.Releases loop
                     if Old.Contains (Release.Version) then
                        Trace.Debug
                          ("Not registering release already indexed: "
                           & Release.Milestone.Image);
                     else
                        Old.Add (Release);
                     end if;
                  end loop;
            end case;

            Old.Merge_Externals (Crate, Policy);

            Contents.Include (Crate.Name, Old);
         end;
      else
         Contents.Insert (Crate.Name, Crate);
      end if;
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (Release : Releases.Release;
                  Policy : Policies.For_Index_Merging :=
                    Policies.Merge_Priorizing_Existing)
   is

      -----------------
      -- Add_Aliases --
      -----------------

      procedure Add_Aliases is
      begin
         for Mil of Release.Provides loop
            declare
               Crate : Releases.Containers.Release_Set :=
                         (if Release_Aliases.Contains (Mil.Crate)
                          then Release_Aliases (Mil.Crate)
                          else Releases.Containers.Empty_Release_Set);

               Providers : Provides.Crate_Providers :=
                             (if Crate_Aliases.Contains (Mil.Crate)
                              then Crate_Aliases (Mil.Crate)
                              else Containers.Crate_Name_Sets.Empty_Set);
            begin
               --  Add release-level aliases

               Crate.Include (Release);
               Release_Aliases.Include (Mil.Crate, Crate);

               --  Add crate-level aliases

               Providers.Include (Release.Name);
               Crate_Aliases.Include (Mil.Crate, Providers);
            end;
         end loop;
      end Add_Aliases;

      Crate : Crates.Crate := Crates.New_Crate (Release.Name);
   begin
      Crate.Add (Release);
      Add (Crate, Policy);

      Add_Aliases;
   end Add;

   package Name_Sets is
     new Ada.Containers.Indefinite_Ordered_Sets (Crate_Name);
   Already_Detected : Name_Sets.Set;

   ----------------------
   -- Detect_Externals --
   ----------------------

   procedure Detect_Externals (Name : Crate_Name; Env : Properties.Vector) is
   begin
      Index_Loading.Load (Name,
                          Detect_Externals => False,
                          Strict           => False);
      --  We don't ask to detect because we are going to do it right after, and
      --  this breaks a potential infinite recursion.

      if Already_Detected.Contains (Name) then
         Trace.Debug
           ("Not redoing detection of externals for crate " & (+Name));
      elsif not Has_Externals (Name) then
         Trace.Debug ("Skipping detection for crate without externals: "
                      & Utils.TTY.Name (Name));
      else
         Already_Detected.Insert (Name);
         Trace.Debug ("Looking for externals for crate: " & (+Name));

         declare
            Providers : Provides.Crate_Providers :=
                          (if Crate_Aliases.Contains (Name)
                           then Crate_Aliases (Name)
                           else Containers.Crate_Name_Sets.Empty_Set);
            --  This copy is needed to avoid tampering with collections if a
            --  new alias were found during detection.
         begin
            Providers.Include (Name);
            --  Always use the crate itself to look for externals

            for Provider of Providers loop
               if Contents.Contains (Provider) then
                  --  It may not exist if this is a virtual crate without
                  --  releases or detectors.

                  if not Contents (Provider).Externals.Is_Empty then
                     Trace.Debug ("Detecting via provider: " &
                                    Utils.TTY.Name (Provider));
                  end if;
                  for Release of Contents (Provider).Externals
                    .Detect (Provider, Env)
                  loop
                     Trace.Debug ("Adding external: "
                                  & Release.Milestone.Image);
                     Add (Release);
                  end loop;
               end if;
            end loop;
         end;
      end if;
   end Detect_Externals;

   ----------------
   -- All_Crates --
   ----------------

   function All_Crates (Opts : Query_Options := Query_Defaults)
                        return access constant Crates.Containers.Maps.Map is
   begin
      if Opts.Load_From_Disk then
         Index_Loading.Load_All.Assert;
      end if;

      return Contents'Access;
   end All_Crates;

   -----------------------
   -- All_Crate_Aliases --
   -----------------------

   function All_Crate_Aliases return access Provides.Crate_Provider_Map
   is (Crate_Aliases'Access);

   -----------
   -- Crate --
   -----------

   function Crate (Name : Crate_Name;
                   Opts : Query_Options := Query_Defaults)
                   return Crates.Crate
   is
   begin
      if Opts.Load_From_Disk then
         Index_Loading.Load (Name,
                             Detect_Externals => Opts.Detect_Externals,
                             Strict => False);
      end if;

      if not Contents.Contains (Name) then
         Raise_Checked_Error ("Requested crate not in index: " & (+Name));
      end if;

      return Contents (Name);
   end Crate;

   -----------------
   -- Crate_Count --
   -----------------

   function Crate_Count return Natural is
     (Natural (Contents.Length));

   ------------
   -- Exists --
   ------------

   function Exists (Name : Crate_Name;
                    Opts : Query_Options := Query_Defaults)
                    return Boolean
   is
   begin
      if Opts.Load_From_Disk then
         Index_Loading.Load (Name,
                             Opts.Detect_Externals,
                             Strict => False);
      end if;

      return Contents.Contains (Name);
   end Exists;

   ------------
   -- Exists --
   ------------

   function Exists (Name : Crate_Name;
                    Version : Semantic_Versioning.Version;
                    Opts : Query_Options := Query_Defaults)
                    return Boolean is
   begin
      if Exists (Name, Opts) then
         for R of Contents (Name).Releases loop
            if R.Name = Name and then R.Version = Version then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end Exists;

   ----------
   -- Find --
   ----------

   function Find (Name : Crate_Name;
                  Version : Semantic_Versioning.Version;
                  Opts    : Query_Options := Query_Defaults) return Release is
   begin
      if Opts.Load_From_Disk then
         Index_Loading.Load (Name,
                             Opts.Detect_Externals,
                             Strict => False);
      end if;

      for R of Contents (Name).Releases loop
         if R.Name = Name and then R.Version = Version then
            return R;
         end if;
      end loop;

      Raise_Checked_Error
        ("Requested milestone not in index: "
         & (+Name) & "=" & Semantic_Versioning.Image (Version));
   end Find;

   -------------------
   -- Has_Externals --
   -------------------

   function Has_Externals (Name : Crate_Name) return Boolean
   is
   begin
      Index_Loading.Load (Name, Detect_Externals => False, Strict => False);

      --  Detectors in the crate itself

      if Contents.Contains (Name) and then
        not Contents (Name).Externals.Is_Empty
      then
         return True;
      end if;

      --  Detectors in other crates that provide it (regular or external)

      if Crate_Aliases.Contains (Name) then
         for Provider of Crate_Aliases (Name) loop
            if Contents.Contains (Provider) and then
              not Contents (Provider).Externals.Is_Empty
            then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end Has_Externals;

   --------------------
   -- Register_Alias --
   --------------------

   procedure Register_Alias (Provider  : Crate_Name;
                             Providing : Crate_Name)
   is
   begin
      if Provider = Providing then
         return;
         --  We don't want the trivial equivalence here as this pollutes the
         --  written file too much.
      end if;

      if Crate_Aliases.Contains (Providing) then
         Crate_Aliases (Providing).Include (Provider);
      else
         Crate_Aliases.Insert
           (Providing,
            Containers.Crate_Name_Sets.To_Set (Provider));
      end if;
   end Register_Alias;

   -------------------
   -- Release_Count --
   -------------------

   function Release_Count return Natural is
   begin
      return Count : Natural := 0 do
         for Crate of Contents loop
            Count := Count + Natural (Crate.Releases.Length);
         end loop;
      end return;
   end Release_Count;

   -------------------------
   -- Releases_Satisfying --
   -------------------------

   function Releases_Satisfying
     (Dep              : Dependencies.Dependency;
      Env              : Properties.Vector;
      Opts             : Query_Options := Query_Defaults;
      Use_Equivalences : Boolean := True;
      Available_Only   : Boolean := True;
      With_Origin      : Origins.Kinds_Set :=
        (others => True))
      return Releases.Containers.Release_Set
   is
      Result : Releases.Containers.Release_Set;
   begin

      --  Regular crates

      if Exists (Dep.Crate, Opts) then
         for Release of Crate (Dep.Crate).Releases loop
            if With_Origin (Release.Origin.Kind)
              and then Release.Satisfies (Dep)
              and then (not Available_Only or else Release.Is_Available (Env))
            then
               Result.Insert (Release);
            end if;
         end loop;
      end if;

      --  And any Release_Aliases via Provides

      if Use_Equivalences and then Release_Aliases.Contains (Dep.Crate) then
         for Release of Release_Aliases (Dep.Crate) loop
            if With_Origin (Release.Origin.Kind)
              and then Release.Satisfies (Dep)
              and then (not Available_Only or else Release.Is_Available (Env))
            then
               Result.Include (Release);
            end if;
         end loop;
      end if;

      return Result;
   end Releases_Satisfying;

end Alire.Index;
