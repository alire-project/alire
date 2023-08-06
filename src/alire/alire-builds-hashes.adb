with Alire.Directories;
with Alire.Hashes.SHA256_Impl;
with Alire.Paths;
with Alire.Roots;
with Alire.Utils.Text_Files;

package body Alire.Builds.Hashes is

   use Directories.Operators;

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
               --  The nul character as separator ensures no ambiguity because
               --  of consecutive entries.
            end loop;

            This.Hashes.Insert (Rel.Name, SHA.Get_Digest (C));
         end Compute_Hash;

         ------------------
         -- Write_Inputs --
         ------------------

         procedure Write_Inputs is
            File : constant Absolute_Path :=
                     Builds.Path
                       / Rel.Base_Folder & "_" & This.Hashes (Rel.Name)
                       / Paths.Working_Folder_Inside_Root
                       / "build_hash_inputs";
            use Directories;
            use Utils.Text_Files;

            Lines : AAA.Strings.Vector;
         begin
            --  First ensure we have a pristine file to work with
            Delete_Tree (File);
            Create_Tree (Parent (File));
            Touch (File);

            --  Now add the hashed contents for the record

            for Var of Vars loop
               Lines.Append (Var);
            end loop;

            Append_Lines (File,
                          Lines,
                          Backup => False);
         end Write_Inputs;

      begin
         Trace.Debug ("   build hashing: " & Rel.Milestone.TTY_Image);

         --  Build profile
         Add ("profile",
              Rel.Name.As_String,
              Root.Configuration.Build_Profile (Rel.Name)'Image);

         --  GPR externals
         --  TBD

         --  Environment variables
         --  TBD

         --  Configuration variables
         --  TBD

         --  Final computation
         Compute_Hash;

         --  Write the hash input for the record
         Write_Inputs;

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
