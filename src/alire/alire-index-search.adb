with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Sets;

with Alire.Conditional;
with Alire.Utils.Tables;
with Alire.Utils.TTY;

with CLIC.TTY;

package body Alire.Index.Search is

   use AAA.Strings;

   ------------------
   -- Print_Crates --
   ------------------

   procedure Print_Crates (Substring : String := "") is
      Table  : Utils.Tables.Table;
      Found  : Natural := 0;
      Lookup : constant String := AAA.Strings.To_Lower_Case (Substring);

      Busy   : Simple_Logging.Ongoing :=
                 Simple_Logging.Activity ("Searching");
   begin
      for Crate of Alire.Index.All_Crates.all loop
         if Lookup = "" or else
           Contains (To_Lower_Case (+Crate.Name), Lookup) or else
           Contains (To_Lower_Case (Crate.Description), Lookup)
         then
            Found := Found + 1;
            Table.New_Row;
            Table.Append (Crate.TTY_Name);
            Table.Append (Crate.TTY_Description);
         end if;
         Busy.Step;
      end loop;

      if Found = 0 then
         Trace.Always ("No hits");
      else
         Table.Print (Always, Separator => "  ");
      end if;
   end Print_Crates;

   ----------------------
   -- Print_Dependents --
   ----------------------

   procedure Print_Dependents (Release    : Releases.Release;
                               Transitive : Boolean;
                               Duplicates : Boolean)
   is
      Table  : Utils.Tables.Table;
      Busy   : Simple_Logging.Ongoing := Simple_Logging.Activity ("Searching");

      --------------
      -- With_Dep --
      --------------
      --  Trick: we replace the release dependencies with the one that causes
      --  this release to become part of the chain, which is the one that
      --  matters. This way we can retrieve it later without requiring new
      --  types and containers.
      function With_Dep (Rel : Releases.Release;
                         Dep : Dependencies.Dependency) return Releases.Release
      is
      begin
         return Result : constant Releases.Release :=
           Rel.Replacing (Dependencies => Conditional.New_Dependency (Dep));
      end With_Dep;

      -----------
      -- Image --
      -----------

      function Image (This : Releases.Containers.Vectors.Vector) return String
      is
         Result : AAA.Strings.Vector;
      begin
         for Link of reverse This loop
            Result.Append (Link.Milestone.TTY_Image);
         end loop;

         return Result.Flatten
           (if CLIC.TTY.Is_TTY then "»" else ",");
      end Image;

      ---------
      -- "<" --
      ---------
      --  The objective of this sorting is having the furthest dependent always
      --  grouped alphabetically (the root of the dependency chain), but then
      --  have shorter chains first, and lexicographic ordering for chains of
      --  the same length.
      function "<" (L, R : Releases.Containers.Vector) return Boolean is
         use type Ada.Containers.Count_Type;
         use type Releases.Release;
      begin
         --  Special check for the first element, which is always compared
         --  based on values. For the remainder, we check length first.

         if "<" (L.Last_Element, R.Last_Element) then
            --  The comparator of Releases says L < R when L has a later
            --  name or newer version, so the result here is reversed.
            return False;

         elsif "<" (R.Last_Element, L.Last_Element) then
            --  Same observation as in the previous case.
            return True;
         end if;

         --  Now length

         if L.Length < R.Length then
            return True;
         elsif L.Length > R.Length then
            return False;
         end if;

         --  And then the rest of the chain in lexicographical order. At this
         --  point we know both have the same length.

         for I in 1 .. Natural (L.Length) - 1 loop -- first already checked
            if "<" (L (L.Last_Index - I), R (R.Last_Index - I)) then
               --  The comparator of Releases says L < R when L has a later
               --  name or newer version, so the result here is reversed.
               return False;

            elsif "<" (R (R.Last_Index - I), L (L.Last_Index - I)) then
               --  Same observation as in the previous case.
               return True;

            end if;
         end loop;

         --  No differences found, so they're equal, neither L < R nor R < L
         return False;
      end "<";

      package Sets is new
        Ada.Containers.Ordered_Sets (Releases.Containers.Vector,
                                     "<" => "<",
                                     "=" => Releases.Containers.Vectors."=");
      Result : Sets.Set;
      --  Stores the actual complete dependency chains

      package Chain_Maps is new
        Ada.Containers.Indefinite_Ordered_Maps
          (Crate_Name,
           Releases.Containers.Vector,
           "=" => Releases.Containers.Vectors."=");
      Shortest_Chains : Chain_Maps.Map;
      --  Shortest known chain for a dependent

      ------------
      -- Length --
      ------------
      --  Shortest chain length known for a crate. Defaults to Positive'Last
      function Length (Crate : Crate_Name) return Positive is
      begin
         if Shortest_Chains.Contains (Crate) then
            return Positive (Shortest_Chains (Crate).Length);
         else
            return Positive'Last;
         end if;
      end Length;

      -----------------------
      -- Gather_Dependents --
      -----------------------
      --  Receives a chain of dep <- dep <- dep dependents, with the first one
      --  being the deepest one, and the last one being the one we must check
      --  who's a dependent on.
      procedure Gather_Dependents (Ancestors : Releases.Containers.Vector) is
         Depth : constant Positive := Positive (Ancestors.Length);
         Dependee : constant Releases.Release := Ancestors.Last_Element;
         use type Releases.Containers.Vector;
      begin

         --  Store a complete chain of dependencies. When no duplicates are
         --  wanted, recursion is interrupted before this point. Since we
         --  recurse depth-first, we can't be sure we find shorter ones first.

         if Depth > 1 then
            if Duplicates then
               --  We want them all
               Result.Insert (Ancestors);
            else
               --  We want the shortest one
               if Length (Dependee.Name) > Depth
                 or else (Length (Dependee.Name) = Depth
                          and then Ancestors < Shortest_Chains (Dependee.Name))
               then
                  Shortest_Chains.Include (Dependee.Name, Ancestors);
               end if;
            end if;
         end if;

         --  Stop looking for dependency chains when only direct dependencies
         --  are requested.

         if Depth > 1 and then not Transitive then
            return;
         end if;

         --  Look for transitive dependents by looking who depends on the last
         --  dependee in the chain.

         for Crate of Alire.Index.All_Crates.all loop
            Busy.Step ("Searching [depth:"
                       & AAA.Strings.Trim (Depth'Image)
                       & "][" & Crate.Name.As_String & "]");

            Release_Loop :
            for Candidate of reverse Crate.Releases loop
               for Dep of Candidate.Flat_Dependencies loop

                  --  Early exit when rejecting duplicates if a shorter chain
                  --  for the candidate dependent already has been found, no
                  --  matter through which intermediate links.

                  exit Release_Loop when
                    not Duplicates
                    and then Length (Candidate.Name) <= Depth;

                  --  This is either a new chain or a shorter one

                  if Dependee.Satisfies (Dep)
                    and then (Transitive or else Depth = 1)
                  then
                     Gather_Dependents
                       (Ancestors & With_Dep (Candidate, Dep));
                     exit Release_Loop;
                  end if;
               end loop;
            end loop Release_Loop;
         end loop;
      end Gather_Dependents;

   begin
      Table.Append ("CRATE").Append ("VERSION").Append ("DEPENDENCY");
      if Transitive then
         Table.Append ("CHAIN");
      end if;
      Table.New_Row;

      Gather_Dependents (Releases.Containers.Vectors.To_Vector (Release, 1));

      --  If no duplicates where requested, copy the result collection to where
      --  the table is printed from.

      if not Duplicates then
         for Chain of Shortest_Chains loop
            Result.Insert (Chain);
         end loop;
      end if;

      --  Final printing

      for Chain of Result loop
         Table
           .Append (Utils.TTY.Name (Chain.Last_Element.Name))
             .Append (TTY.Version (Chain.Last_Element.Version.Image))
               .Append (TTY.Version (Chain (2) -- The one depending on the 1st
                        .Flat_Dependencies     -- only one dep due to With_Dep
                        .First_Element.Versions.Synthetic_Image));

         if Transitive then
            Table.Append (Image (Chain));
         end if;

         Table.New_Row;
      end loop;

      Table.Print (Always, Separator => "  ");
   end Print_Dependents;

end Alire.Index.Search;
