with Alire.Directories;
with Alire.Environment;
with Alire.GPR;
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

      Env : Environment.Env_Map;

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
            Vars.Include (Datum);
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

         -----------------
         -- Add_Profile --
         -----------------

         procedure Add_Profile is
         begin
            Add ("profile",
                 Rel.Name.As_String,
                 Root.Configuration.Build_Profile (Rel.Name)'Image);
         end Add_Profile;

         -------------------
         -- Add_Externals --
         -------------------

         procedure Add_Externals is
            Externals : constant Releases.Externals_Info := Rel.GPR_Externals;
         begin
            for Var of GPR.Name_Vector'(Externals.Declared
                                .Union (Externals.Modified))
              --  Externals modified but not declared are presumably for the
              --  benefit of another crate. It's unclear if these will affect
              --  the crate doing the setting, so we err on the side of
              --  caution and include them in the hashing. Maybe we could make
              --  this inclusion dependent on some config variable, or push
              --  responsibility to crate maintainers to declare all externals
              --  that affect the own crate properly and remove them from the
              --  hashing inputs.
            loop
               if Env.Contains (Var) then
                  Add ("external", Var, Env (Var));
               else
                  Add ("external", Var, "default");
               end if;
            end loop;
         end Add_Externals;

         ------------------
         -- Add_Compiler --
         ------------------

         procedure Add_Compiler is
            --  Compiler version. Changing compiler will result in incompatible
            --  ALI files and produce rebuilds, so it must be part of the hash.
            --  Incidentally, this serves to separate by cross-target too.
            Compiler : constant Releases.Release := Root.Compiler;
         begin
            Add ("version", Compiler.Name.As_String, Compiler.Version.Image);
         end Add_Compiler;

         ---------------------
         -- Add_Environment --
         ---------------------

         procedure Add_Environment is
         begin
            for Var of Rel.Environment (Root.Environment) loop
               --  If the crate modifies the var, it must be in the loaded env
               Add ("environment", Var.Name, Env (Var.Name));
            end loop;
         end Add_Environment;

      begin
         Trace.Debug ("   build hashing: " & Rel.Milestone.TTY_Image);

         --  Add individual contributors to the hash input
         Add_Profile;
         Add_Externals;
         Add_Environment;
         Add_Compiler;

         --  Configuration variables
         --  TBD

         --  Dependencies recursive hash? Since a crate can use a dependency
         --  config spec, it is possible in the worst case for a crate to
         --  require unique builds that include their dependencies hash
         --  in their own hash. This is likely a corner case, but we can't
         --  currently detect it. Two options are to alway err on the side of
         --  caution, always including dependencies hashes, or to add some new
         --  info in the manifest saying whose crates config affect the crate.
         --  We could also enable this recursive hashing globally or per
         --  crate...
         --  TBD

         --  Final computation
         Compute_Hash;

         --  Write the hash input for the record
         Write_Inputs;

         Trace.Debug ("   build hashing release complete");
      end Compute;

      Context : Environment.Context;

   begin
      Trace.Debug ("build hashing root " & Root.Path);
      This.Hashes.Clear;

      Environment.Load (Context, Root, For_Hashing => True);
      Env := Context.Get_All;

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
