with Alire.Hashes.SHA256_Impl;
with Alire.Roots;

package body Alire.Builds.Hashes is

   package SHA renames Alire.Hashes.SHA256_Impl;

   subtype Variables is AAA.Strings.Set;
   --  We'll store all variables that affect a Release in a deterministic order

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Hasher) is
   begin
      This.Hashes.Clear;
   end Clear;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Hasher) return Boolean
   is (This.Hashes.Is_Empty);

   -------------
   -- Compute --
   -------------

   procedure Compute (This : in out Hasher;
                      Root : in out Roots.Root)
   is

      -------------
      -- Compute --
      -------------

      procedure Compute (Rel : Releases.Release) is
         Vars : Variables;

         ---------
         -- Add --
         ---------

         procedure Add (Kind, Key, Value : String) is
            use AAA.Strings;
            Datum : constant String :=
                      Trim (Kind) & ":"
                      & Trim (Key) & "="
                      & Trim (Value);
         begin
            Trace.Debug ("      build hashing " & Datum);
            Vars.Insert (Datum);
         end Add;

         ------------------
         -- Compute_Hash --
         ------------------

         procedure Compute_Hash is
            C : SHA.Hashing_Context;
         begin
            for Var of Vars loop
               SHA.Update (C, Var, Append_Nul => True);
            end loop;

            This.Hashes.Insert (Rel.Name, SHA.Get_Digest (C));
         end Compute_Hash;

      begin
         Trace.Debug ("   build hashing: " & Rel.Milestone.TTY_Image);

         --  Build profile
         Add ("profile", "profile",
              Root.Configuration.Build_Profile (Rel.Name)'Image);

         --  GPR externals
         --  TBD

         --  Environment variables
         --  TBD

         --  Configuration variables
         --  TBD

         --  Final computation
         Compute_Hash;

         Trace.Debug ("   build hashing release complete");
      end Compute;

   begin
      Trace.Debug ("build hashing root " & Root.Path);
      This.Hashes.Clear;

      for Rel of Root.Solution.Releases loop
         if Root.Requires_Build_Sync (Rel) then
            Compute (Rel);
         end if;
      end loop;
   end Compute;

   ----------
   -- Hash --
   ----------

   function Hash (This : in out Hasher;
                  Name : Crate_Name)
                  return String
   is (This.Hashes (Name));

end Alire.Builds.Hashes;
