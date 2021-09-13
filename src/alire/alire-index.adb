with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

with Alire.Containers;

with Alire.Utils.TTY;

package body Alire.Index is

   package Release_Set_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Crate_Name, Releases.Containers.Release_Set,
        "<",        Releases.Containers."=");
   subtype Release_Alias_Map is Release_Set_Maps.Map;

   package External_Alias_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (Crate_Name,
                                             Containers.Crate_Name_Sets.Set,
                                             "<",
                                             Containers.Crate_Name_Sets."=");
   subtype External_Alias_Map is External_Alias_Maps.Map;

   use all type Semantic_Versioning.Version;

   Contents : aliased Alire.Crates.Containers.Maps.Map;
   --  Regular mapping from crate name to its releases

   Aliases  : Release_Alias_Map;
   --  Mapping from crate name to any release that satisfies it. Currently,
   --  releases are duplicated in memory. These two collections could be made
   --  to share releases via some indirection or pointers.

   External_Aliases : External_Alias_Map;
   --  For external crates that provide another crate, we need to be aware
   --  when external detection is requested. This mapping goes in the direction
   --  Provided -> Providers.

   ---------
   -- Add --
   ---------

   procedure Add (Crate  : Crates.Crate;
                  Policy : Policies.For_Index_Merging :=
                    Policies.Merge_Priorizing_Existing) is
   begin
      if Exists (Crate.Name) then
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
                         (if Aliases.Contains (Mil.Crate)
                          then Aliases (Mil.Crate)
                          else Releases.Containers.Empty_Release_Set);
            begin
               Crate.Include (Release);
               Aliases.Include (Mil.Crate, Crate);
            end;
         end loop;
      end Add_Aliases;

      Crate : Crates.Crate := Crates.New_Crate (Release.Name);
   begin
      Crate.Add (Release);
      Add (Crate, Policy);

      Add_Aliases;
   end Add;

   --------------------------
   -- Detect_All_Externals --
   --------------------------

   procedure Detect_All_Externals (Env : Properties.Vector) is
   begin
      Trace.Detail ("Detecting external releases...");

      for Crate of Contents loop
         Detect_Externals (Crate.Name, Env);
      end loop;
   end Detect_All_Externals;

   package Name_Sets is
     new Ada.Containers.Indefinite_Ordered_Sets (Crate_Name);
   Already_Detected : Name_Sets.Set;

   ----------------------
   -- Detect_Externals --
   ----------------------

   procedure Detect_Externals (Name : Crate_Name; Env : Properties.Vector) is
   begin
      if Already_Detected.Contains (Name) then
         Trace.Debug
           ("Not redoing detection of externals for crate " & (+Name));
      elsif not External_Aliases.Contains (Name) then
         Trace.Debug ("Skipping detection for crate without externals: "
                      & Utils.TTY.Name (Name));
      else
         Already_Detected.Insert (Name);
         Trace.Debug ("Looking for externals for crate: " & (+Name));

         for Provider of External_Aliases (Name) loop
            Trace.Debug ("Detecting via provider " &
                           Utils.TTY.Name (Provider));
            for Release of Contents (Provider).Externals.Detect (Provider, Env)
            loop
               Trace.Debug ("Adding external: " & Release.Milestone.Image);
               Add (Release);
            end loop;
         end loop;
      end if;
   end Detect_Externals;

   ----------------
   -- All_Crates --
   ----------------

   function All_Crates return access constant Crates.Containers.Maps.Map is
     (Contents'Access);

   -----------
   -- Crate --
   -----------

   function Crate (Name : Crate_Name) return Crates.Crate
   is (Contents (Name));

   -----------------
   -- Crate_Count --
   -----------------

   function Crate_Count return Natural is
     (Natural (Contents.Length));

   ------------
   -- Exists --
   ------------

   function Exists (Name : Crate_Name) return Boolean is
     (Contents.Contains (Name));

   ------------
   -- Exists --
   ------------

   function Exists (Name : Crate_Name;
                    Version : Semantic_Versioning.Version)
                    return Boolean is
   begin
      if Exists (Name) then
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
                  Version : Semantic_Versioning.Version) return Release is
   begin
      for R of Contents (Name).Releases loop
         if R.Name = Name and then R.Version = Version then
            return R;
         end if;
      end loop;

      raise Checked_Error with
        "Requested milestone not in index: "
        & (+Name) & "=" & Semantic_Versioning.Image (Version);
   end Find;

   -------------------
   -- Has_Externals --
   -------------------

   function Has_Externals (Name : Crate_Name) return Boolean
   is (External_Aliases.Contains (Name));

   -----------------------------
   -- Register_External_Alias --
   -----------------------------

   procedure Register_External_Alias (Provider  : Crate_Name;
                                      Providing : Crate_Name)
   is
   begin
      if External_Aliases.Contains (Providing) then
         External_Aliases (Providing).Include (Provider);
      else
         External_Aliases.Insert
           (Providing,
            Containers.Crate_Name_Sets.To_Set (Provider));
      end if;
   end Register_External_Alias;

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

   function Releases_Satisfying (Dep              : Dependencies.Dependency;
                                 Env              : Properties.Vector;
                                 Use_Equivalences : Boolean := True;
                                 Available_Only   : Boolean := True;
                                 With_Origin      : Origins.Kinds_Set :=
                                   (others => True))
                                 return Releases.Containers.Release_Set
   is
      Result : Releases.Containers.Release_Set;
   begin

      --  Regular crates

      if Exists (Dep.Crate) then
         for Release of Crate (Dep.Crate).Releases loop
            if With_Origin (Release.Origin.Kind)
              and then Release.Satisfies (Dep)
              and then (not Available_Only or else Release.Is_Available (Env))
            then
               Result.Insert (Release);
            end if;
         end loop;
      end if;

      --  And any aliases via Provides

      if Use_Equivalences and then Aliases.Contains (Dep.Crate) then
         for Release of Aliases (Dep.Crate) loop
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
