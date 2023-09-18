with Alire.Crate_Configuration.Hashes;
with Alire.Directories;
with Alire.Environment.Loading;
with Alire.Errors;
with Alire.GPR;
with Alire.Hashes.SHA256_Impl;
with Alire.Paths;
with Alire.Roots;
with Alire.Solutions;
with Alire.Dependencies.States;
with Alire.Utils.Text_Files;

package body Alire.Builds.Hashes is

   use Directories.Operators;

   package SHA renames Alire.Hashes.SHA256_Impl;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Hasher) is
   begin
      This.Hashes.Clear;
      This.Inputs.Clear;
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

            --  We can rarely reach here more than once for provided regular
            --  releases, so we cannot simply insert once. Instead of including
            --  blindly, we double-check things match.
            if This.Hashes.Contains (Rel.Name)
              and then This.Hashes (Rel.Name) /= SHA.Get_Digest (C)
            then
               raise Program_Error with
                 "Conflicting build hashes for release "
                 & Rel.Milestone.Image;
            else
               This.Hashes.Include (Rel.Name, SHA.Get_Digest (C));
               This.Inputs.Include (Rel.Name, Vars);
            end if;
         end Compute_Hash;

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

            function Set (S : String) return GPR.Name_Vector
            is (GPR.To_Set (S));

            Hardcoded : constant GPR.Name_Vector
              := Set ("LIBRARY_TYPE")
              .Union (Set (AAA.Strings.To_Upper_Case (Rel.Name_Str)
                      & "_LIBRARY_TYPE"));
            --  We add these to our generated library crates, although they're
            --  not declared anywhere in manifests.
         begin
            for Var of GPR.Name_Vector'(Externals.Declared
                                .Union (Externals.Modified)
                                .Union (Hardcoded))
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

         -----------------------
         -- Add_Configuration --
         -----------------------

         procedure Add_Configuration is
         begin
            Crate_Configuration.Hashes.Add_From
              (Config => Root.Configuration.all,
               Rel    => Rel,
               Add    => Add'Access);
         end Add_Configuration;

         ----------------------
         -- Add_Dependencies --
         ----------------------

         procedure Add_Dependencies is
            --  Since specs of dependencies may change due to config variables,
            --  or sources selected depending on environment variables/GPR
            --  externals, the safest course of action is to also include
            --  dependency hashes in our own hash. For dependencies without
            --  such things the hash won't change anyway.
         begin
            for Dep of Rel.Flat_Dependencies loop
               for Target of Root.Solution.Releases loop
                  if Target.Origin.Requires_Build
                    and then Target.Satisfies (Dep)
                  then
                     if This.Contains (Target.Name) then
                        Add ("dependency",
                             Target.Milestone.Image,
                             This.Hashes (Target.Name));
                     else
                        raise Program_Error with Errors.Set
                          (Rel.Milestone.Image & " depends on "
                           & Target.Milestone.Image
                           & " but hash is not yet computed?");
                     end if;
                  end if;
               end loop;
            end loop;
         end Add_Dependencies;

      begin
         Trace.Debug ("   build hashing: " & Rel.Milestone.TTY_Image);

         --  Add individual contributors to the hash input
         Add_Profile;       -- Build profile
         Add_Configuration; -- Crate configuration variables

         --  These are only relevant for shared dependencies, as they don't
         --  appear in the contents of generated files. Not including them
         --  allows things to work as they were for sandboxed dependencies.
         if not Builds.Sandboxed_Dependencies then
            Add_Externals;     -- GPR externals
            Add_Environment;   -- Environment variables

            --  In the root crate we can skip compiler detection, as it has no
            --  bearing on the hash or config regeneration. This allows most
            --  operations in a crate without dependencies to succeed even in
            --  absence of a configured compiler.
            if not Root.Is_Root_Release (Rel.Name) then
               Add_Compiler;   -- Compiler version
            end if;

            Add_Dependencies;  -- Hash of dependencies
         end if;

         --  Final computation
         Compute_Hash;

         Trace.Debug ("   build hashing release complete");
      end Compute;

      -------------
      -- Compute --
      -------------

      procedure Compute (unused_Root  : in out Roots.Root;
                         Unused_Sol   : Solutions.Solution;
                         State        : Dependencies.States.State)
      is
      begin
         if State.Has_Release
           and then State.Release.Origin.Requires_Build
         then
            --  We cannot filter out provided dependencies because all
            --  dependencies may be fulfilled by a provided release that
            --  doesn't appear otherwise (as non-provided).
            Compute (State.Release);
         end if;
      end Compute;

      Context : Environment.Context;

   begin
      Trace.Debug ("build hashing root " & Root.Path);
      This.Hashes.Clear;

      Environment.Loading.Load (Context, Root, For_Hashing => True);
      Env := Context.Get_All;

      Root.Configuration.Ensure_Complete;

      Root.Traverse (Compute'Access);
   end Compute;

   ----------------------
   -- Inputs_File_Name --
   ----------------------

   function Inputs_File_Name (Root : in out Roots.Root;
                              Rel  : Releases.Release)
                              return Absolute_Path
   is (Root.Release_Base (Rel.Name, Roots.For_Build)
       / Paths.Working_Folder_Inside_Root
       / "build_hash_inputs");

   ------------------
   -- Write_Inputs --
   ------------------

   procedure Write_Inputs (This : Hasher;
                           Root : in out Roots.Root)
   is

      ------------------
      -- Write_Inputs --
      ------------------

      procedure Write_Inputs (Rel : Releases.Release) is
         File : constant Absolute_Path := Inputs_File_Name (Root, Rel);
         use Directories;
         use Utils.Text_Files;
      begin
         --  First ensure we have a pristine file to work with
         Delete_Tree (File);
         Create_Tree (Parent (File));
         Touch (File);

         --  Now add the hashed contents for the record

         Append_Lines (File,
                       This.Inputs (Rel.Name).To_Vector,
                       Backup => False);
      end Write_Inputs;

      ------------------
      -- Write_Inputs --
      ------------------

      procedure Write_Inputs (Unused_Root  : in out Roots.Root;
                              Unused_Sol   : Solutions.Solution;
                              State        : Dependencies.States.State)
      is
      begin
         if State.Has_Release
           and then State.Release.Origin.Requires_Build
         then
            Write_Inputs (State.Release);
         end if;
      end Write_Inputs;

   begin
      Root.Traverse (Write_Inputs'Access);
   end Write_Inputs;

   ----------
   -- Hash --
   ----------

   function Hash (This : in out Hasher;
                  Name : Crate_Name)
                  return String
   is (This.Hashes (Name));

   ------------
   -- Inputs --
   ------------

   function Inputs (This : Hasher;
                   Name : Crate_Name)
                   return Variables
   is (This.Inputs (Name));

   -------------------
   -- Stored_Inputs --
   -------------------

   function Stored_Inputs (Root : in out Roots.Root;
                           Rel  : Releases.Release)
                           return Variables
   is
      Name : constant Absolute_Path := Inputs_File_Name (Root, Rel);
   begin
      if Directories.Is_File (Name) then
         declare
            File : Utils.Text_Files.File :=
                     Utils.Text_Files.Load (Name);
            Lines : constant AAA.Strings.Vector := File.Lines.all;
         begin
            return Lines.To_Set;
         end;
      else
         return AAA.Strings.Empty_Set;
      end if;
   end Stored_Inputs;

end Alire.Builds.Hashes;
