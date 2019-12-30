package body Alire.Index is

   use all type Semantic_Versioning.Version;

   Crates : aliased Projects.Containers.Maps.Map;

   ---------
   -- Add --
   ---------

   procedure Add (Crate  : Projects.With_Releases.Crate;
                  Policy : Addition_Policies := Merge_Priorizing_Existing) is
      pragma Unreferenced (Policy);
   begin
      if Exists (Crate.Name) then
         declare
            Old : Projects.With_Releases.Crate := Crates (Crate.Name);
         begin
            for Release of Crate.Releases loop
               if Old.Contains (Release.Version) then
                  Trace.Debug ("Not registering release already indexed: "
                               & Release.Milestone.Image);
               else
                  Old.Add (Release);
               end if;
            end loop;

            Crates.Include (Crate.Name, Old);
         end;
      else
         Crates.Insert (Crate.Name, Crate);
      end if;
   end Add;

   ----------------
   -- All_Crates --
   ----------------

   function All_Crates return access constant Projects.Containers.Maps.Map is
     (Crates'Access);

   -----------
   -- Crate --
   -----------

   function Crate (Name : Crate_Name) return Projects.With_Releases.Crate
   is (Crates (Name));

   -----------------
   -- Crate_Count --
   -----------------

   function Crate_Count return Natural is
     (Natural (Crates.Length));

   ------------
   -- Exists --
   ------------

   function Exists (Project : Crate_Name) return Boolean is
     (Crates.Contains (Project));

   ------------
   -- Exists --
   ------------

   function Exists (Project : Crate_Name;
                    Version : Semantic_Versioning.Version)
                    return Boolean is
   begin
      if Exists (Project) then
         for R of Crates (Project).Releases loop
            if R.Project = Project and then R.Version = Version then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end Exists;

   ----------
   -- Find --
   ----------

   function Find (Project : Crate_Name;
                  Version : Semantic_Versioning.Version) return Release is
   begin
      for R of Crates (Project).Releases loop
         if R.Project = Project and then R.Version = Version then
            return R;
         end if;
      end loop;

      raise Checked_Error with
        "Requested milestone not in index: "
        & (+Project) & "=" & Semantic_Versioning.Image (Version);
   end Find;

   -------------------
   -- Release_Count --
   -------------------

   function Release_Count return Natural is
   begin
      return Count : Natural := 0 do
         for Crate of Crates loop
            Count := Count + Natural (Crate.Releases.Length);
         end loop;
      end return;
   end Release_Count;

end Alire.Index;
