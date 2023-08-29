with Alire.Crate_Configuration.Hashes;
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
            This.Inputs.Insert (Rel.Name, Vars);
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

         -----------------------
         -- Add_Configuration --
         -----------------------

         procedure Add_Configuration is
         begin
            Crate_Configuration.Hashes.Add_From
              (Config => Root.Configuration,
               Rel    => Rel,
               Add    => Add'Access);
         end Add_Configuration;

      begin
         Trace.Debug ("   build hashing: " & Rel.Milestone.TTY_Image);

         --  Add individual contributors to the hash input
         Add_Profile;       -- Build profile
         Add_Externals;     -- GPR externals
         Add_Environment;   -- Environment variables
         Add_Compiler;      -- Compiler version
         Add_Configuration; -- Crate configuration variables

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

         Trace.Debug ("   build hashing release complete");
      end Compute;

      Context : Environment.Context;

   begin
      Trace.Debug ("build hashing root " & Root.Path);
      This.Hashes.Clear;

      Environment.Load (Context, Root, For_Hashing => True);
      Env := Context.Get_All;

      Root.Configuration.Ensure_Complete;

      for Rel of Root.Solution.Releases.Including (Root.Release) loop
         --  We need to hash the root release to be able to check for changes
         --  in the root crate configuration.

         if Rel.Origin.Requires_Build then
            Compute (Rel);
         end if;
      end loop;
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

   begin
      for Rel of Root.Solution.Releases.Including (Root.Release) loop
         if Rel.Origin.Requires_Build then
            Write_Inputs (Rel);
         end if;
      end loop;
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
         return AAA.Strings.Empty;
      end if;
   end Stored_Inputs;

end Alire.Builds.Hashes;
