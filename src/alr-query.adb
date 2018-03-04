with Alire.Dependencies.Vectors;
with Alire.Utils;

with Alr.Checkout;
with Alr.Platform;

package body Alr.Query is

   package Semver renames Semantic_Versioning;

   use all type Semver.Version_Set;

   ----------------------
   -- Dependency_Image --
   ----------------------

   function Dependency_Image (Project  : Project_Name;
                              Versions : Semantic_Versioning.Version_Set;
                              Policy   : Policies := Newest) return String is
      (Project &
       (if Versions /= Semver.Any
        then " version " & Semver.Image (Versions)
        else " with " & Utils.To_Mixed_Case (Policy'Img) & " version"));

   ------------
   -- Exists --
   ------------

   function Exists (Project : Project_Name;
                    Allowed : Semantic_Versioning.Version_Set := Semantic_Versioning.Any)
                    return Boolean
   is
      use Semver;
   begin
      for R of Index.Catalog loop
         if R.Project = Project and then Satisfies (R.Version, Allowed) then
            return True;
         end if;
      end loop;

      return False;
   end Exists;

   ----------
   -- Find --
   ----------

   function Find (Project : Project_Name;
                  Allowed : Semantic_Versioning.Version_Set := Semantic_Versioning.Any;
                  Policy  : Policies) return Release
   is
      use Semantic_Versioning;

      -----------
      -- Check --
      -----------

      function Check (R : Index.Release) return Boolean is
      begin
         if R.Project = Project then
            if Satisfies (R.Version, Allowed) then
               return True;
            else
               Trace.Debug ("Skipping unsatisfactory version: " & Image (R.Version));
            end if;
         end if;

         return False;
      end Check;

   begin
      if Policy = Newest then
         for R of reverse Index.Catalog loop
            if Check (R) then
               return R;
            end if;
         end loop;
      else
         for R of Index.Catalog loop
            if Check (R) then
               return R;
            end if;
         end loop;
      end if;

      raise Query_Unsuccessful with "Release not found: " & Project;
   end Find;

   --------------------
   -- Print_Solution --
   --------------------

   procedure Print_Solution (I : Instance) is
      use Containers.Project_Release_Maps;
   begin
      for Rel of I loop
         Log ("  " & Rel.Milestone.Image, Debug);
      end loop;
   end Print_Solution;

   -------------
   -- Resolve --
   -------------

   function Resolve (Unresolved :        Alire.Dependencies.Vectors.Vector;
                     Frozen     :        Instance;
                     Policy     :        Policies;
                     Success    : in out Boolean) return Instance
   is
      subtype Dependencies is Alire.Dependencies.Vectors.Vector;

      --  FIXME: since this is depth-first, Frozen can be passed in-out and updated on the spot,
      --  thus saving copies. Probably the same applies to Unresolved.
      Dep : constant Alire.Dependencies.Dependency :=
                 (if Unresolved.Is_Empty
                  then Alire.Dependencies.New_Dependency ("fake", Semver.Any)
                  else Unresolved.First_Element);
      --  The fake project will never be referenced, since the first check is that unresolved is empty
      --  we are done

      Remain : Alire.Dependencies.Vectors.Vector := Unresolved;

      -----------
      -- Check --
      -----------

      function Check (R : Release) return Instance is
      begin
         if Dep.Project = R.Project and Then
            Semver.Satisfies (R.Version, Dep.Versions) and then
            Checkout.Available_Currently (R)
         then
            declare
               New_Frozen : Instance     := Frozen;
               New_Remain : Dependencies := Remain;

               Solution   : Instance;
            begin
               New_Frozen.Insert (R.Project, R);
               New_Remain.Append (R.Depends (Platform.Properties));

               Solution := Resolve (New_Remain, New_Frozen, Policy, Success);

               if not Solution.Is_Empty then
                  return Solution; -- Success!
               end if;
            end;
         end if;

         return Empty_Instance;
      end Check;

   begin

      if Unresolved.Is_Empty then
         Log ("Dependencies resolved", Detail);
         Print_Solution (Frozen);
         Success := True;
         return Frozen;
      end if;

      Remain.Delete_First;

      if Frozen.Contains (Dep.Project) then
         if Semver.Satisfies (Frozen.Element (Dep.Project).Version, Dep.Versions) then
            --  Dependency already met, simply go down...
            return Resolve (Remain, Frozen, Policy, Success);
         else
            --  Failure because an already frozen version is incompatible
            return Empty_Instance;
         end if;
      else
         -- Need to check all versions for the first one...
         -- FIXME: complexity can be improved not visiting blindly all releases to match by project
         if Policy = Newest then
            for R of reverse Index.Catalog loop
               declare
                  Solution : constant Instance := Check (R);
               begin
                  if not Solution.Is_Empty then
                     return Solution;
                  end if;
               end;
            end loop;
         else
            for R of Index.Catalog loop
               declare
                  Solution : constant Instance := Check (R);
               begin
                  if not Solution.Is_Empty then
                     return Solution;
                  end if;
               end;
            end loop;
         end if;

         Trace.Detail ("Unable to find release for dependency: " & Dep.Image);

         --  We found no milestone compatible with the first unresolved dependency...
         return Empty_Instance;
      end if;
   end Resolve;

   -------------
   -- Resolve --
   -------------

   function Resolve (Deps    :     Alire.Types.Platform_Dependencies;
                     Success : out Boolean;
                     Policy  :     Policies) return Instance is
   begin
      Success := False;

      if Deps.Is_Empty then
         Success := True;
         return Empty_Instance;
      end if;

      return I : constant Instance := Resolve (Deps,
                                               Containers.Project_Release_Maps.Empty_Map,
                                               Policy,
                                               Success)
      do
         if not Success then
            Trace.Debug ("Dependency resolution failed");
         end if;
      end return;
   end Resolve;

end Alr.Query;
