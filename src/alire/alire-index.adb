with Ada.Containers.Indefinite_Ordered_Sets;

package body Alire.Index is

   use all type Semantic_Versioning.Version;

   Contents : aliased Alire.Crates.Containers.Maps.Map;

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
      Crate : Crates.Crate := Crates.New_Crate (Release.Name);
   begin
      Crate.Add (Release);
      Add (Crate, Policy);
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
      elsif not Exists (Name) then
         Trace.Debug ("Skipping external detection for unindexed crate");
      else
         Already_Detected.Insert (Name);
         Trace.Debug ("Looking for externals for crate: " & (+Name));
         for Release of Contents (Name).Externals.Detect (Name, Env) loop
            Trace.Debug ("Adding external: " & Release.Milestone.Image);
            Contents (Name).Add (Release);
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

   function Releases_Satisfying (Dep : Dependencies.Dependency)
                                 return Containers.Release_Set
   is
      Result : Containers.Release_Set;
   begin
      if Exists (Dep.Crate) then
         for Release of Crate (Dep.Crate).Releases loop
            if Release.Satisfies (Dep) then
               Result.Insert (Release);
            end if;
         end loop;
      end if;

      return Result;
   end Releases_Satisfying;

end Alire.Index;
