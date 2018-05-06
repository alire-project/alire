with Alire.Conditional;
with Alire.Utils;

with Alr.Commands;
with Alr.Parsers;
with Alr.Platform;

package body Alr.Query is

   package Semver renames Semantic_Versioning;

   use all type Semver.Version_Set;

   ----------------------
   -- Dependency_Image --
   ----------------------

   function Dependency_Image (Project  : Alire.Project;
                              Versions : Semantic_Versioning.Version_Set;
                              Policy   : Policies := Newest) return String is
      ((+Project) &
       (if Versions /= Semver.Any
        then " version " & Semver.Image (Versions)
        else " with " & Utils.To_Mixed_Case (Policy'Img) & " version"));

   ------------
   -- Exists --
   ------------

   function Exists (Project : Alire.Project;
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

   function Find (Project : Alire.Project;
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

      raise Query_Unsuccessful with "Release not found: " & (+Project);
   end Find;

   ----------
   -- Find --
   ----------

   function Find (Project : String;
                  Policy  : Policies) return Release
   is
      Spec : constant Parsers.Allowed_Milestones := Parsers.Project_Versions (Project);
   begin
      return Find (Spec.Project,
                   Spec.Versions,
                   Policy);
   end Find;

   ------------------
   -- Is_Available --
   ------------------

   function Is_Available (R : Alire.Index.Release) return Boolean is
     (R.Available.Check (Platform.Properties) and then
          (if R.Origin.Is_Native
           then Origins.New_Origin (R.Origin).Exists));

   -------------------
   -- Is_Resolvable --
   -------------------

   function Is_Resolvable (Deps : Types.Platform_Dependencies) return Boolean is
      (Resolve (Deps, Commands.Query_Policy).Valid);

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

   function Resolve (Unresolved : Types.Platform_Dependencies;
                     Frozen     : Instance;
                     Policy     : Policies) return Solution
   is

      ----------------------
      -- Solve_Dependency --
      ----------------------

      function Solve_Dependency (Dep : Types.Dependency) return Solution is

         -----------
         -- Check --
         -----------

         function Check (R : Release) return Solution is
            use Alire.Containers;
         begin
            if Dep.Project = R.Project and then
              Semver.Satisfies (R.Version, Dep.Versions) and then
              Is_Available (R)
            then
               Trace.Debug ("SOLVER: frozen dependency: " & R.Milestone.Image &
                              " that adds" & R.Depends (Platform.Properties).Leaf_Count'Img &
                              " further dependencies");
               declare
                  Sol : constant Solution :=
                          Resolve (R.Depends (Platform.Properties),
                                   Frozen.Inserting (To_Map (R)),
                                   Policy);
               begin
                  if Sol.Valid then
                     Trace.Debug ("SOLVER: consolidated dependencies of: " & R.Milestone.Image);
                     return (True,
                             To_Map (R).Inserting (Sol.Releases));
                  else
                     Trace.Debug ("SOLVER: failed dependencies of: " & R.Milestone.Image);
                     return (Valid => False);
                  end if;
               end;
            else
               return (Valid => False);
            end if;
         end Check;

      begin
         if Frozen.Contains (Dep.Project) then
            if Semver.Satisfies (Frozen.Element (Dep.Project).Version, Dep.Versions) then
               --  Dependency already met
               return (True, Empty_Instance);
            else
               --  Failure because an already frozen version is incompatible
               return (Valid => False);
            end if;
         else
            -- FIXME: use Floor/Ceiling or cleverer data structure to not blindly visit all releases
            if Policy = Newest then
               for R of reverse Index.Catalog loop
                  declare
                     Sol : constant Solution := Check (R);
                  begin
                     if Sol.Valid then
                        return Sol;
                     end if;
                  end;
               end loop;
            else
               for R of Index.Catalog loop
                  declare
                     Sol : constant Solution := Check (R);
                  begin
                     if Sol.Valid then
                        return Sol;
                     end if;
                  end;
               end loop;
            end if;
         end if;

         Trace.Detail ("Unable to find release for dependency: " & Dep.Image);
         return (Valid => False);
      end Solve_Dependency;

      ----------------------
      -- Solve_And_Vector --
      ----------------------

      function Solve_And_Vector (Unresolved : Types.Platform_Dependencies) return Solution is
        New_Sol : Instance := Empty_Instance;
      begin
         for I in Unresolved.Iterate loop
            declare
               Sol : constant Solution := Resolve (Unresolved (I),
                                                   Frozen.Inserting (New_Sol),
                                                   Policy);
            begin
               if not Sol.Valid then
                  return (Valid => False);
               end if;

               --  Merge whatever was solved underneath
               New_Sol.Insert (Sol.Releases);
            end;
         end loop;

         return (True, New_Sol);
      end Solve_And_Vector;

      ---------------------
      -- Solve_Or_Vector --
      ---------------------

      function Solve_Or_Vector (Unresolved : Types.Platform_Dependencies) return Solution is
      begin
         for I in Unresolved.Iterate loop
            declare
               Sol : constant Solution := Resolve (Unresolved (I),
                                                   Frozen,
                                                   Policy);
            begin
               if Sol.Valid then
                  return Sol;
               end if;
            end;
         end loop;

         return (Valid => False);
      end Solve_Or_Vector;

      use Alire.Conditional.For_Dependencies;

   begin
      if Unresolved.Is_Empty then
         return (True, Empty_Instance);
      end if;

      case Unresolved.Kind is
         when Value =>
            return Solve_Dependency (Unresolved.Value);
         when Vector =>
            if Unresolved.Conjunction = Anded then
               return Solve_And_Vector (Unresolved);
            else
               return Solve_Or_Vector (Unresolved);
            end if;
         when Condition =>
            raise Program_Error
              with "Requisites should be evaluated prior to Resolve";
      end case;
   end Resolve;

   -------------
   -- Resolve --
   -------------

   function Resolve (Deps   : Types.Platform_Dependencies;
                     Policy : Policies) return Solution is
   begin
      if Deps.Is_Empty then
         return (True, Empty_Instance);
      end if;

      return Sol : constant Solution := Resolve (Deps,
                                                 Empty_Instance,
                                                 Policy)
      do
         if Sol.Valid then
            Trace.Debug ("Dependencies solved with" &
                           Sol.Releases.Length'Img & " releases");
         else
            Trace.Debug ("Dependency resolution failed");
         end if;
      end return;
   end Resolve;

end Alr.Query;
