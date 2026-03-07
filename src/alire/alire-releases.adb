with Ada.Directories;
with Ada.Text_IO;

with Alire.Settings.Builtins;
with Alire.Crates;
with Alire.Directories;
with Alire.Defaults;
with Alire.Errors;
with Alire.Flags;
with Alire.Formatting;
with Alire.Origins.Deployers.System;
with Alire.Paths;
with Alire.Properties.Bool;
with Alire.Properties.From_TOML;
with Alire.Properties.Scenarios;
with Alire.TOML_Load;
with Alire.Utils.Tables;
with Alire.Utils.YAML;
with Alire.Warnings;

with GNAT.IO; -- To keep preelaborable

with Semantic_Versioning.Basic;
with Semantic_Versioning.Extended;

with TOML.File_IO;
with Ada.Strings.Fixed;

package body Alire.Releases is

   package Semver renames Semantic_Versioning;

   use all type Alire.Properties.Labeled.Labels;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Release) return Boolean
   is (if L.Provides (GNAT_Crate) and then R.Provides (GNAT_Crate)
       then Sort_Compilers (L, R)
       else Standard_Sorting (L, R));

   --------------------
   -- All_Properties --
   --------------------

   function All_Properties (R : Release;
                            P : Alire.Properties.Vector)
                            return Alire.Properties.Vector
   is (Materialize (R.Properties, P));

   ------------------------
   -- Default_Properties --
   ------------------------

   function Default_Properties return Conditional.Properties
   is (Conditional.For_Properties.New_Value
       (New_Label (Description,
                   Defaults.Description)));

   -----------------------
   -- Flat_Dependencies --
   -----------------------

   function Flat_Dependencies
     (R : Release;
      P : Alire.Properties.Vector := Alire.Properties.No_Properties)
      return Alire.Dependencies.Containers.List
   is
   begin
      if P.Is_Empty then
         --  Trying to evaluate a tree with empty dependencies will result
         --  in spurious warnings about missing environment properties (as we
         --  indeed didn't give any). Since we want to get flat dependencies
         --  that do not depend on any properties, this is indeed safe to do.
         return Conditional.Enumerate (R.Dependencies);
      else
         return Conditional.Enumerate (R.Dependencies.Evaluate (P));
      end if;
   end Flat_Dependencies;

   -------------------------
   -- Check_Caret_Warning --
   -------------------------
   --  Warn of ^0.x dependencies that probably should be ~0.x
   function Check_Caret_Warning (This : Release) return Boolean is
      Newline    : constant String := ASCII.LF & "   ";
   begin
      for Dep of This.Flat_Dependencies loop
         if Settings.Builtins.Warning_Caret.Get
           and then
           AAA.Strings.Contains (Dep.Versions.Image, "^0")
         then
            Warnings.Warn_Once
              ("Possible tilde intended instead of caret for a 0.x version."
               & Newline
               & "Alire does not change the meaning of caret and tilde"
               & " for pre/post-1.0 versions."
               & Newline
               & "The suspicious dependency is: " & TTY.Version (Dep.Image)
               & Newline
               & "You can disable this warning by setting the option "
               & TTY.Emph (Settings.Builtins.Warning_Caret.Key) & " to false.",
               Warnings.Caret_Or_Tilde);
            return True;
         end if;
      end loop;

      return False;
   end Check_Caret_Warning;

   -------------------
   -- Dependency_On --
   -------------------

   function Dependency_On (R     : Release;
                           Crate : Crate_Name;
                           P     : Alire.Properties.Vector :=
                             Alire.Properties.No_Properties)
                           return Alire.Dependencies.Containers.Optional
   is
   begin
      for Dep of R.Flat_Dependencies (P) loop
         if Dep.Crate = Crate then
            return Alire.Dependencies.Containers.Optionals.Unit (Dep);
         end if;
      end loop;

      return Alire.Dependencies.Containers.Optionals.Empty;
   end Dependency_On;

   -----------------
   -- Base_Folder --
   -----------------

   function Base_Folder (R : Release) return Relative_Path
   is
      use Directories.Operators;
   begin
      if R.Origin.Is_Monorepo then
         return R.Deployment_Folder / R.Origin.Subdir;
      else
         return R.Deployment_Folder;
      end if;
   end Base_Folder;

   -----------------------
   -- Deployment_Folder --
   -----------------------

   function Deployment_Folder (R : Release) return Folder_String
   is
      use all type Origins.Kinds;

      -------------------
      -- Monorepo_Path --
      -------------------

      function Monorepo_Path return Relative_Path is
         --  For a monorepo we want to reuse the checkout, so instead of the
         --  name of the release we use the simple name of the URL, no version
         --  (as a monorepo may contain differently versioned crates) and the
         --  commit ID.

         --------------
         -- Sanitize --
         --------------
         --  Repository names may still contain problematic chars, we replace
         --  all of those with a '_'.
         function Sanitize (Name : String) return String is
         begin
            return Safe : String := Name do
               for I in Safe'Range loop
                  if Safe (I) & "" not in Folder_String then
                     Safe (I) := '_';
                  end if;
               end loop;
            end return;
         end Sanitize;

         -----------------
         -- Simple_Name --
         -----------------

         function Simple_Name (URL : String) return String is
            --  For local repos, on Windows, we may find '\' in the URL, so
            --  here we get as simple name of a repo whatever is after the
            --  last '/' or '\'.
         begin
            for I in reverse URL'Range loop
               if URL (I) in '\' | '/' | ':' then
                  return URL (I + 1 .. URL'Last);
               end if;
            end loop;

            Raise_Checked_Error ("Malformed URL: " & URL);
         end Simple_Name;

      begin
         return
           Sanitize (Ada.Directories.Base_Name (Simple_Name (R.Origin.URL)))
           & "_"
           & (case R.Origin.Kind is
                 when Git | Hg => R.Origin.Short_Unique_Id,
                 when SVN => R.Origin.Commit,
                 when others => raise Program_Error with
                   "monorepo folder only applies to VCS origins");
      end Monorepo_Path;

      ------------------
      -- Release_Path --
      ------------------

      function Release_Path return Relative_Path
      is (
          --  Name of the release
          R.Name_Str & "_" &

         --  Version without pre-release/build strings
            AAA.Strings.Head
            (AAA.Strings.Head (Image (R.Version), '-'), '+') & "_" &
          --  Remove patch/build strings that may violate folder valid chars

          --  Unique hash when available
          (case R.Origin.Kind is
                when Binary_Archive => R.Origin.Short_Unique_Id,
                when External       => "external",
                when Filesystem     => "filesystem",
                when System         => "system",
                when Source_Archive => R.Origin.Short_Unique_Id,
                when Git | Hg       => R.Origin.Short_Unique_Id,
                when SVN            => R.Origin.Commit));

   begin
      if R.Origin.Is_Monorepo then
         return Monorepo_Path;
      else
         return Release_Path;
      end if;
   end Deployment_Folder;

   ------------
   -- Deploy --
   ------------

   procedure Deploy
     (This            : Alire.Releases.Release;
      Env             : Alire.Properties.Vector;
      Parent_Folder   : String;
      Was_There       : out Boolean;
      Create_Manifest : Boolean := False;
      Include_Origin  : Boolean := False;
      Mark_Completion : Boolean := True)
   is
      use Alire.Directories;
      Repo_Folder : constant Any_Path :=
                      Parent_Folder / This.Deployment_Folder;
      Rel_Folder  : constant Any_Path :=
                      Parent_Folder / This.Base_Folder;
      Completed   : Flags.Flag := Flags.Complete_Copy (Repo_Folder);

      ------------------------------
      -- Backup_Upstream_Manifest --
      ------------------------------

      procedure Backup_Upstream_Manifest is
         Working_Dir : Guard (Enter (Rel_Folder)) with Unreferenced;
      begin
         Ada.Directories.Create_Path (Paths.Working_Folder_Inside_Root);

         if GNAT.OS_Lib.Is_Regular_File (Paths.Crate_File_Name) then
            Trace.Debug ("Backing up bundled manifest file at "
                         & Adirs.Current_Directory & " as *.upstream");
            declare
               Upstream_File : constant String :=
                                 Paths.Working_Folder_Inside_Root
                                 / (Paths.Crate_File_Name & ".upstream");
            begin
               --  Backup if already there
               Alire.Directories.Backup_If_Existing
                 (Upstream_File,
                  Base_Dir => Paths.Working_Folder_Inside_Root);
               --  Remove just backed up file
               if Directories.Is_File (Upstream_File) then
                  Directories.Delete_Tree
                    (Directories.Full_Name (Upstream_File));
               end if;
               --  And rename the original manifest into upstream
               Directories.Rename
                 (Source      => Paths.Crate_File_Name,
                  Destination => Upstream_File);
            end;
         end if;
      end Backup_Upstream_Manifest;

      -----------------------------------
      -- Create_Authoritative_Manifest --
      -----------------------------------

      procedure Create_Authoritative_Manifest (Kind : Manifest.Sources) is
      begin
         Trace.Debug ("Generating manifest file for "
                      & This.Milestone.TTY_Image & " with"
                      & This.Dependencies.Leaf_Count'Img & " dependencies "
                      & " at " & (Rel_Folder / Paths.Crate_File_Name));

         This.Whenever (Env).To_File (Rel_Folder / Paths.Crate_File_Name,
                                      Kind);
      end Create_Authoritative_Manifest;

   begin

      Trace.Debug ("Deploying " & This.Milestone.TTY_Image
                   & " into " & TTY.URL (Repo_Folder));

      --  Deploy if the target dir is not already there. We only skip for
      --  releases that require a folder to be deployed; system releases
      --  require the deploy attempt as the installation check is done by
      --  the deployer.

      if This.Origin.Is_Index_Provided and then Completed.Exists then
         Was_There := True;
         Trace.Detail ("Skipping checkout of already available " &
                         This.Milestone.Image);

      elsif This.Origin.Kind not in Origins.Deployable_Kinds then
         Was_There := True;
         Trace.Detail ("External requires no deployment for " &
                         This.Milestone.Image);

      elsif This.Origin.Is_System
        and then Origins.Deployers.System.Already_Installed (This.Origin)
      then
         Was_There := True;
         Trace.Detail ("Skipping install of already available system origin " &
                         This.Milestone.Image);

      else
         Was_There := False;
         Put_Info ("Deploying " & This.Milestone.TTY_Image & "...");
         Alire.Origins.Deployers.Deploy (This, Repo_Folder).Assert;
      end if;

      --  For deployers that do nothing, we ensure the folder exists so all
      --  dependencies leave a trace in the cache/dependencies folder, and
      --  a place from where to run their actions by default.

      Ada.Directories.Create_Path (Repo_Folder);

      --  Backup a potentially packaged manifest, so our authoritative
      --  manifest from the index is always used.

      Backup_Upstream_Manifest;

      --  Create manifest if requested

      if Create_Manifest then
         Create_Authoritative_Manifest (if Include_Origin
                                        then Manifest.Index
                                        else Manifest.Local);
      end if;

      if Mark_Completion then
         Completed.Mark (Done => True);
      end if;

   exception
      when E : others =>
         --  Clean up if deployment failed after the initial deployment (e.g.
         --  during an action).
         Log_Exception (E);

         if Ada.Directories.Exists (Repo_Folder) then
            Trace.Debug ("Cleaning up failed release deployment of "
                         & This.Milestone.TTY_Image);
            Directories.Force_Delete (Repo_Folder);
         end if;

         raise;
   end Deploy;

   ----------------------------
   -- Install_System_Package --
   ----------------------------

   procedure Install_System_Package (This : Release) is
   begin
      Origins.Deployers.System.Install (This);
   end Install_System_Package;

   ----------------
   -- Forbidding --
   ----------------

   function Forbidding (Base      : Release;
                        Forbidden : Conditional.Forbidden_Dependencies)
                        return Release is
   begin
      return Extended : Release := Base do
         Extended.Forbidden := Forbidden;
      end return;
   end Forbidding;

   ---------------
   -- Providing --
   ---------------

   function Providing (Base    : Release;
                       Targets : Containers.Crate_Name_Sets.Set)
                       return Release
   is
   begin
      return Result : Release := Base do
         for Target of Targets loop
            Result.Equivalences.Append
              (Milestones.New_Milestone (Target, Base.Version));
         end loop;
      end return;
   end Providing;

   ---------------
   -- Replacing --
   ---------------

   function Replacing (Base   : Release;
                       Origin : Origins.Origin) return Release is
   begin
      return Replaced : Release := Base do
         Replaced.Origin := Origin;
      end return;
   end Replacing;

   ---------------
   -- Replacing --
   ---------------

   function Replacing
     (Base         : Release;
      Dependencies : Conditional.Dependencies := Conditional.No_Dependencies)
      return Release is
   begin
      return Replaced : Release := Base do
         Replaced.Dependencies := Dependencies;
      end return;
   end Replacing;

   ---------------
   -- Replacing --
   ---------------

   function Replacing
     (Base         : Release;
      Properties   : Conditional.Properties   := Conditional.No_Properties)
      return Release is
   begin
      return Replaced : Release := Base do
         Replaced.Properties := Properties;
      end return;
   end Replacing;

   ---------------
   -- Replacing --
   ---------------

   function Replacing
     (Base               : Release;
      Notes              : Description_String := "")
      return Release
   is
      New_Notes   : constant Description_String := (if Notes = ""
                                                    then Base.Notes
                                                    else Notes);
   begin

      return Replacement : constant Release
        (Base.Name.Length, New_Notes'Length) :=
        (Prj_Len   => Base.Name.Length,
         Notes_Len => New_Notes'Length,
         Name      => Base.Name,
         Notes     => New_Notes,

         Version      => Base.Version,
         Origin       => Base.Origin,
         Dependencies => Base.Dependencies,
         Equivalences => Base.Equivalences,
         Pins         => Base.Pins,
         Forbidden    => Base.Forbidden,
         Properties   => Base.Properties,
         Available    => Base.Available,
         Imported     => Base.Imported)
      do
         null;
      end return;
   end Replacing;

   ---------------
   -- Retagging --
   ---------------

   function Retagging (Base    : Release;
                       Version : Semantic_Versioning.Version) return Release is
   begin
      return Upgraded : Release := Base do
         Upgraded.Version := Version;
      end return;
   end Retagging;

   ---------------
   -- Upgrading --
   ---------------

   function Upgrading (Base    : Release;
                       Version : Semantic_Versioning.Version;
                       Origin  : Origins.Origin) return Release is
   begin
      return Upgraded : Release := Base do
         Upgraded.Version := Version;
         Upgraded.Origin  := Origin;
      end return;
   end Upgrading;

   -----------------
   -- New_Release --
   -----------------

   function New_Release (Name         : Crate_Name;
                         Version      : Semantic_Versioning.Version;
                         Origin       : Origins.Origin;
                         Notes        : Description_String;
                         Dependencies : Conditional.Dependencies;
                         Properties   : Conditional.Properties;
                         Available    : Conditional.Availability)
                         return Release
   is (Prj_Len      => Name.Length,
       Notes_Len    => Notes'Length,
       Name         => Name,
       Version      => Version,
       Origin       => Origin,
       Notes        => Notes,
       Dependencies => Dependencies,
       Equivalences => <>,
       Pins         => <>,
       Forbidden    => Conditional.For_Dependencies.Empty,
       Properties   => Properties,
       Available    => Available,
       Imported     => No_TOML_Value);

   -----------------------
   -- New_Empty_Release --
   -----------------------

   function New_Empty_Release (Name : Crate_Name) return Release
   is (New_Working_Release (Name         => Name,
                            Properties   => Conditional.No_Properties));

   -------------------------
   -- New_Working_Release --
   -------------------------

   function New_Working_Release
     (Name         : Crate_Name;
      Origin       : Origins.Origin           := Origins.New_Filesystem (".");
      Dependencies : Conditional.Dependencies :=
        Conditional.For_Dependencies.Empty;
      Properties   : Conditional.Properties   :=
        Default_Properties)
      return         Release is
     (Prj_Len      => Name.Length,
      Notes_Len    => 0,
      Name         => Name,
      Version      => +"0.0.0",
      Origin       => Origin,
      Notes        => "",
      Dependencies => Dependencies,
      Equivalences => <>,
      Pins         => <>,
      Forbidden    => Conditional.For_Dependencies.Empty,
      Properties   => Properties,
      Available    => Conditional.Empty,
      Imported     => No_TOML_Value
     );

   -------------------------
   -- On_Platform_Actions --
   -------------------------

   function On_Platform_Actions (R : Release;
                                 P : Alire.Properties.Vector;
                                 Moments : Moment_Array := (others => True))
                                 return Alire.Properties.Vector
   is
      use Alire.Properties.Actions;
   begin
      return Filtered : Alire.Properties.Vector do
         for Prop of R.On_Platform_Properties
           (P, Alire.Properties.Actions.Action'Tag)
         loop
            if Moments (Action'Class (Prop).Moment) then
               Filtered.Append (Prop);
            end if;
         end loop;
      end return;
   end On_Platform_Actions;

   ----------------------------
   -- On_Platform_Properties --
   ----------------------------

   function On_Platform_Properties
     (R             : Release;
      P             : Alire.Properties.Vector;
      Descendant_Of : Ada.Tags.Tag := Ada.Tags.No_Tag)
      return Alire.Properties.Vector
   is
      use Ada.Tags;
   begin
      if Descendant_Of = No_Tag then
         return Materialize (R.Properties, P);
      else
         declare
            Props : constant Alire.Properties.Vector :=
              R.On_Platform_Properties (P);
         begin
            return Result : Alire.Properties.Vector do
               for P of Props loop
                  if Is_Descendant_At_Same_Level (P'Tag, Descendant_Of) then
                     Result.Append (P);
                  end if;
               end loop;
            end return;
         end;
      end if;
   end On_Platform_Properties;

   ----------------------
   -- Props_To_Strings --
   ----------------------

   function Props_To_Strings (Props : Alire.Properties.Vector;
                    Label : Alire.Properties.Labeled.Labels)
                    return AAA.Strings.Vector is
      --  Extract values of a particular label
      Filtered : constant Alire.Properties.Vector :=
                   Alire.Properties.Labeled.Filter (Props, Label);
   begin
      return Strs : AAA.Strings.Vector do
         for P of Filtered loop
            Strs.Append (Alire.Properties.Labeled.Label (P).Value);
         end loop;
      end return;
   end Props_To_Strings;

   -----------------
   -- Environment --
   -----------------

   function Environment (R : Release;
                         P : Alire.Properties.Vector) return Env_Maps.Map
   is
      package Env renames Alire.Properties.Environment;
   begin
      return Map : Env_Maps.Map do
         for Prop of R.On_Platform_Properties (P, Env.Variable'Tag) loop
            Map.Insert (Env.Variable (Prop).Name,
                        Env.Variable (Prop));
         end loop;
      end return;
   end Environment;

   -----------------
   -- Executables --
   ----------------

   function Executables (R : Release;
                         P : Alire.Properties.Vector :=
                           Platforms.Current.Properties)
                         return AAA.Strings.Vector
   is
      Exes : constant AAA.Strings.Vector :=
        Props_To_Strings (R.All_Properties (P), Executable);
   begin
      if OS_Lib.Exe_Suffix /= "" then
         declare
            With_Suffix : AAA.Strings.Vector;
         begin
            for I in Exes.Iterate loop
               With_Suffix.Append (Exes (I) & OS_Lib.Exe_Suffix);
            end loop;
            return With_Suffix;
         end;
      end if;
      return Exes;
   end Executables;

   -------------------
   -- GPR_Externals --
   -------------------

   function GPR_Externals (R : Release;
                                     P : Alire.Properties.Vector :=
                                       Platforms.Current.Properties)
                                     return Externals_Info
   is
   begin
      return Result : Externals_Info do
         for Prop of R.On_Platform_Properties
           (P, Alire.Properties.Scenarios.Property'Tag)
         loop
            declare
               Var : Alire.Properties.Scenarios.Property'Class renames
                       Alire.Properties.Scenarios.Property'Class (Prop);
            begin
               case Var.Value.Kind is
                  when GPR.Enumeration | GPR.Free_String =>
                     --  This is a declaration of an external that affects R
                     Result.Declared.Include (Var.Value.Name);
                  when GPR.External =>
                     Result.Modified.Include (Var.Value.Name);
               end case;
            end;
         end loop;
      end return;
   end GPR_Externals;

   -------------------
   -- Project_Files --
   -------------------

   function Project_Files (R         : Release;
                           P         : Alire.Properties.Vector;
                           With_Path : Boolean)
                           return AAA.Strings.Vector
   is
      use AAA.Strings;
      use all type Origins.Kinds;

      With_Paths : AAA.Strings.Vector :=
        Props_To_Strings (R.All_Properties (P), Project_File);
      Without    : AAA.Strings.Vector;
   begin
      if With_Paths.Is_Empty
        and then
         R.Origin.Kind not in Binary_Archive | External | System
      then
         --  Default project file if no one is specified by the crate. Only if
         --  the create is not binary, external nor system.
         With_Paths.Append (String'((+R.Name) & ".gpr"));
      end if;

      if With_Path then
         return With_Paths;
      else
         for File of With_Paths loop

            --  Basename
            Without.Append (Split (Text => File,
                                   Separator => '/',
                                   Side      => Tail,
                                   From      => Tail,
                                   Raises    => False));

         end loop;

         return Without;
      end if;
   end Project_Files;

   -------------------
   -- Project_Paths --
   -------------------

   function Project_Paths (R         : Release;
                           P         : Alire.Properties.Vector)
                           return      AAA.Strings.Set
   is
      use Ada.Strings;
      use AAA.Strings;

      Files : constant AAA.Strings.Vector :=
        Project_Files (R, P, With_Path => True);
   begin
      return Paths : AAA.Strings.Set do
         for File of Files loop
            if Contains (File, "/") then
               Paths.Include
                 (File (File'First .. Fixed.Index (File, "/", Backward)));
            else

               --  The project file is at the root of the release
               Paths.Include ("");
            end if;
         end loop;
      end return;
   end Project_Paths;

   -------------------
   -- Auto_GPR_With --
   -------------------

   function Auto_GPR_With (R : Release) return Boolean is
      Vect : constant Alire.Properties.Vector :=
        Conditional.Enumerate (R.Properties).Filter
        (Alire.TOML_Keys.Auto_GPR_With);
   begin
      if not Vect.Is_Empty then
         return Alire.Properties.Bool.Property (Vect.First_Element).Value;
      else
         --  The default is to enable auto-gpr-with
         return True;
      end if;
   end Auto_GPR_With;

   ------------------
   -- Has_Property --
   ------------------

   function Has_Property (R : Release; Key : String) return Boolean
   is (for some Prop of Conditional.Enumerate (R.Properties) =>
          Prop.Key = AAA.Strings.To_Lower_Case (Key));

   ------------------------
   -- Labeled_Properties --
   ------------------------

   function Labeled_Properties_Vector (R     : Release;
                                       P     : Alire.Properties.Vector;
                                       Label : Alire.Properties.Labeled.Labels)
                                       return Alire.Properties.Vector is
   begin
      return Alire.Properties.Labeled.Filter (R.All_Properties (P), Label);
   end Labeled_Properties_Vector;

   ------------------------
   -- Labeled_Properties --
   ------------------------

   function Labeled_Properties (R     : Release;
                                P     : Alire.Properties.Vector;
                                Label : Alire.Properties.Labeled.Labels)
                                return AAA.Strings.Vector
   is
   begin
      return Props_To_Strings (R.All_Properties (P), Label);
   end Labeled_Properties;

   -----------
   -- Print --
   -----------

   procedure Print (R : Release) is
      use GNAT.IO;
   begin
      if Alire.Utils.Tables.Structured_Output then
         if R.Imported.Is_Present then
            --  This field may be missing if R.Whenever has been used, in which
            --  case we properly want to print the re-exported information
            --  without dynamic expressions (else branch below). It may be also
            --  missing for releases being created from scratch during `alr
            --  init`, but there's no way for a user to get us here until
            --  after the release has been reloaded from its manifest.
            Formatting.Print (R.Imported.all);
         else
            if R.Properties.Is_Unconditional then
               Formatting.Print
                 (R.To_TOML
                    (if R.Origin.Kind in Origins.Filesystem
                     then Manifest.Local
                     else Manifest.Index));
            else
               --  Shouldn't happen as conditional releases should have the
               --  Imported field populated (they always come from a loaded
               --  manifest).
               raise Program_Error with
                 "Cannot export release with dynamic information";
            end if;
         end if;
         return;
      end if;

      --  MILESTONE
      Put_Line (R.Milestone.TTY_Image & ": " & R.TTY_Description);

      if not R.Equivalences.Is_Empty then
         Put_Line ("Provides: " & R.Equivalences.Image_One_Line);
      end if;

      if R.Notes /= "" then
         Put_Line ("Notes: " & R.Notes);
      end if;

      --  ORIGIN
      Put_Line ("Origin: " & R.Origin.Image);

      --  AVAILABILITY
      if not R.Available.Is_Empty then
         Put_Line ("Available when: " & R.Available.Image_One_Line);
      end if;

      --  PROPERTIES
      if not R.Properties.Is_Empty then
         Put_Line ("Properties:");
         R.Properties.Print ("   ",
                             And_Or  => False,
                             Verbose => Alire.Log_Level >= Detail);
      end if;

      --  DEPENDENCIES
      if not R.Dependencies.Is_Empty then
         Put_Line ("Dependencies (direct):");
         R.Dependencies.Print ("   ",
                               Sorted => True,
                               And_Or => R.Dependencies.Contains_ORs);
      end if;

      --  PINS
      if not R.Pins.Is_Empty then
         Put_Line ("Pins (direct):");
         R.Pins.Print (Prefix => "   ");
      end if;
   end Print;

   --------------
   -- Property --
   --------------

   function Property (R   : Release;
                      Key : Alire.Properties.Labeled.Labels)
                      return String
   is
      use Alire.Properties.Labeled;
      Target : constant Alire.Properties.Vector :=
                 Filter (R.Properties, Key);
   begin
      if Target.Length not in 1 then
         raise Constraint_Error with
           "Unexpected property count:" & Target.Length'Img;
      end if;

      return Label (Target.First_Element).Value;
   end Property;

   -----------------------
   -- Property_Contains --
   -----------------------

   function Property_Contains (R : Release; Str : String) return Boolean is
      use AAA.Strings;

      Search : constant String := To_Lower_Case (Str);
   begin
      for P of Conditional.Enumerate (R.Properties) loop
         declare
            Text : constant String :=
                     To_Lower_Case
                       ((if Contains (P.Image, ":")
                        then Tail (P.Image, ':')
                        else P.Image));
         begin
            if Contains (Text, Search) then
               return True;
            end if;
         end;
      end loop;

      return False;
   end Property_Contains;

   -----------------------
   -- Property_Contains --
   -----------------------

   function Property_Contains (R : Release; Str : String)
                               return AAA.Strings.Set
   is
      Results : AAA.Strings.Set;
      use AAA.Strings;

      Search : constant String := To_Lower_Case (Str);
   begin
      for P of Conditional.Enumerate (R.Properties) loop
         declare
            Image : constant String := P.Image;
         begin
            if Contains (Image, ":") then
               declare
                  Prop  : constant String := Head (Image, ':');
                  Value : constant String := Trim (Tail (Image, ':'));
               begin
                  if Contains (To_Lower_Case (Value), Search) then
                     Results.Include (Prop);
                  end if;
               end;
            else
               if Contains (To_Lower_Case (Image), Search) then
                  Results.Include (Image);
               end if;
            end if;
         end;
      end loop;

      return Results;
   end Property_Contains;

   -------------------
   -- From_Manifest --
   -------------------

   function From_Manifest (File_Name : Any_Path;
                           Source    : Manifest.Sources;
                           Strict    : Boolean;
                           Root_Path : Any_Path := "")
                           return Release
   is
      --  Move to file base dir, as relative paths in pins are resolved during
      --  loading relative to CWD.
      CWD : Directories.Guard
        (if Root_Path /= "" then
            Directories.Enter (Root_Path)
         else
            Directories.Stay) with Unreferenced;
   begin
      return From_TOML
        (TOML_Adapters.From
           (TOML_Load.Load_File (File_Name),
            "Loading release from manifest: " & File_Name),
         Source,
         Strict,
         File_Name);
   exception
      when E : others =>
         Log_Exception (E);
         --  As this file is edited manually, it may not load for many reasons
         Raise_Checked_Error (Errors.Wrap ("Failed to load " & File_Name,
                                           Errors.Get (E)));
   end From_Manifest;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (From   : TOML_Adapters.Key_Queue;
                       Source : Manifest.Sources;
                       Strict : Boolean;
                       File   : Any_Path := "")
                       return Release is
   begin
      From.Assert_Key (TOML_Keys.Name, TOML.TOML_String);

      return This : Release := New_Empty_Release
        (Name => +From.Unwrap.Get (TOML_Keys.Name).As_String)
      do
         --  Keep the original TOML to be able to export with conditional
         --  expressions unresolved. Keep a copy since TOML is using reference
         --  semantics.

         This.Imported.all := From.Unwrap.Clone;

         --  Extract the version ASAP to show it properly during logging

         if From.Contains (TOML_Keys.Version) then
            This := This.Retagging
              (Version => Semver.Parse
                 (From.Unwrap.Get (TOML_Keys.Version).As_String,
                  Relaxed => True));
         end if;

         Assert (This.From_TOML (From, Source, Strict, File));
      end return;
   end From_TOML;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (This   : in out Release;
                       From   :        TOML_Adapters.Key_Queue;
                       Source :        Manifest.Sources;
                       Strict :        Boolean;
                       File   :        Any_Path := "")
                       return Outcome
   is
      package Dirs    renames Ada.Directories;
      package Labeled renames Alire.Properties.Labeled;
   begin
      Trace.Debug ("Loading release " & This.Milestone.Image);

      --  Origin

      case Source is
         when Manifest.Index =>
            This.Origin.From_TOML (From).Assert;
         when Manifest.Local =>
            This.Origin :=
              Origins.New_Filesystem
                (Dirs.Containing_Directory   -- same folder as manifest file's
                   (Dirs.Full_Name (File))); -- absolute path
            --  We don't require an origin for a local release, as the release
            --  is already in place.
      end case;

      --  Properties

      TOML_Load.Load_Crate_Section
        (Strict  => Strict or else Source in Manifest.Local,
         Section => (case Source is
                        when Manifest.Index => Crates.Index_Release,
                        when Manifest.Local => Crates.Local_Release),
         From    => From,
         Props   => This.Properties,
         Deps    => This.Dependencies,
         Equiv   => This.Equivalences,
         Forbids => This.Forbidden,
         Pins    => This.Pins,
         Avail   => This.Available);

      --  Consolidate/validate some properties as fields:

      Assert (This.Name_Str = This.Property (Labeled.Name),
              "Mismatched name property and given name at release creation");

      This.Version := Semver.New_Version (This.Property (Labeled.Version));

      --  Check for remaining keys, which must be erroneous:
      return From.Report_Extra_Keys;
   end From_TOML;

   -------------------
   -- To_Dependency --
   -------------------

   function To_Dependency (R : Release) return Conditional.Dependencies is
     (Conditional.For_Dependencies.New_Value
        (Alire.Dependencies.New_Dependency
             (R.Name,
              Semver.Extended.To_Extended
                (Semver.Basic.Exactly (R.Version)))));

   -------------
   -- To_File --
   -------------

   procedure To_File (R        : Release;
                      Filename : String;
                      Format   : Manifest.Sources) is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Filename);
      TOML.File_IO.Dump_To_File (R.To_TOML (Format), File);
      Close (File);
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end To_File;

   -------------
   -- To_TOML --
   -------------

   function To_TOML (R      : Release;
                     Format : Manifest.Sources)
                     return TOML.TOML_Value
   is
      use all type Alire.Properties.From_TOML.Cardinalities;
      use TOML_Adapters;
      function Tomlify is
        new Tomify_Enum (Alire.Properties.From_TOML.Property_Keys);
      Root : constant TOML.TOML_Value := R.Properties.To_TOML;
   begin

      --  Name
      Root.Set (TOML_Keys.Name, +R.Name_Str);

      --  Version
      Root.Set (TOML_Keys.Version, +Semver.Image (R.Version));

      --  Provided equivalences
      if not R.Equivalences.Is_Empty then
         Root.Set (TOML_Keys.Provides, R.Equivalences.To_TOML);
      end if;

      --  Notes
      if R.Notes'Length > 0 then
         Root.Set (TOML_Keys.Notes, +R.Notes);
      end if;

      --  Ensure atoms are atoms and arrays are arrays
      for Prop in Alire.Properties.From_TOML.Cardinality'Range loop
         declare
            Toml_Key : constant String := Tomlify (Prop).As_String;
         begin
            if Root.Has (Toml_Key) then
               case Alire.Properties.From_TOML.Cardinality (Prop) is
                  when Unique =>
                     Assert
                       (Root.Get (Toml_Key).Kind in
                            TOML.Atom_Value_Kind
                          | TOML.TOML_Table,
                        "Expected unique value/table for key '" & Toml_Key
                        & "' but is: " & Root.Get (Toml_Key).Kind'Image,
                        Unchecked => True);
                     --  Unchecked as it shouldn't happen if manifest reading
                     --  did its checks as intended.
                  when Multiple =>
                     Root.Set
                       (Toml_Key,
                        TOML_Adapters.To_Array (Root.Get (Toml_Key)));
               end case;
            end if;
         end;
      end loop;

      --  Origin
      case Format is
         when Manifest.Index =>
            Root.Set (TOML_Keys.Origin, R.Origin.To_TOML);
         when Manifest.Local =>
            null;
      end case;

      --  Dependencies, wrapped as an array
      if not R.Dependencies.Is_Empty then
         declare
            Dep_Array : constant TOML.TOML_Value := TOML.Create_Array;
         begin
            Dep_Array.Append (R.Dependencies.To_TOML);
            Root.Set (TOML_Keys.Depends_On, Dep_Array);
         end;
      end if;

      --  Forbidden, wrapped as an array
      if not R.Forbidden.Is_Empty then
         declare
            Forbid_Array : constant TOML.TOML_Value := TOML.Create_Array;
         begin
            Forbid_Array.Append (R.Forbidden.To_TOML);
            Root.Set (TOML_Keys.Forbidden, Forbid_Array);
         end;
      end if;

      --  Available
      if R.Available.Is_Empty or else
         R.Available.Value.Is_Available
      then
         null; -- Do nothing, do not pollute .toml file
      else
         Root.Set (TOML_Keys.Available, R.Available.To_TOML);
      end if;

      return Root;
   end To_TOML;

   -------------
   -- To_YAML --
   -------------

   overriding
   function To_YAML (R : Release) return String is

      function Props_To_YAML
      is new Utils.YAML.To_YAML (Alire.Properties.Property'Class,
                                 Alire.Properties.Vectors,
                                 Alire.Properties.Vector);

      Deps : constant String := R.Dependencies.To_YAML;
   begin
      return
        "crate: " & Utils.YAML.YAML_Stringify (R.Name_Str) & ASCII.LF &
        "authors: " & Props_To_YAML (R.Author) & ASCII.LF &
        "maintainers: " & Props_To_YAML (R.Maintainer) & ASCII.LF &
        "licenses: " & Props_To_YAML (R.License) & ASCII.LF &
        "websites: " & Props_To_YAML (R.Website) & ASCII.LF &
        "tags: " & Props_To_YAML (R.Tag) & ASCII.LF &
        "version: " & Utils.YAML.YAML_Stringify (R.Version_Image) & ASCII.LF &
        "short_description: " & Utils.YAML.YAML_Stringify (R.Description) &
        ASCII.LF &

        "dependencies: " &  (if Deps'Length = 0
                               or else
                                Deps (Deps'First) /= '['
                             then
                                --  Add array brackets when there's only one
                                --  dependency or no dependency.
                                "[" & Deps & "]"
                             else
                                Deps) & ASCII.LF &

        "configuration_variables: " &
           Props_To_YAML (R.Config_Variables) & ASCII.LF &
        "configuration_values: " &
           Props_To_YAML (R.Config_Settings) & ASCII.LF;
   end To_YAML;

   -------------
   -- Version --
   -------------

   function Version (R : Release) return Semantic_Versioning.Version is
     (R.Version);

   --------------
   -- Whenever --
   --------------

   function Whenever (R : Release;
                      P : Alire.Properties.Vector)
                      return Release
   is (Prj_Len      => R.Prj_Len,
       Notes_Len    => R.Notes_Len,
       Name         => R.Name,
       Version      => R.Version,
       Origin       => R.Origin.Whenever (P),
       Notes        => R.Notes,
       Dependencies => R.Dependencies.Evaluate (P),
       Equivalences => R.Equivalences,
       Pins         => R.Pins,
       Forbidden    => R.Forbidden.Evaluate (P),
       Properties   => R.Properties.Evaluate (P),
       Available    => R.Available.Evaluate (P),

       Imported     => No_TOML_Value
       --  We are discarding information above, so the imported information
       --  would no longer match.
      );

   ----------------------
   -- Long_Description --
   ----------------------

   function Long_Description (R : Release) return String is
      Descr : constant Alire.Properties.Vector :=
        Conditional.Enumerate (R.Properties).Filter
        (Alire.TOML_Keys.Long_Descr);
   begin
      if not Descr.Is_Empty then
         --  Image returns "Description: Blah" so we have to cut.
         return AAA.Strings.Tail (Descr.First_Element.Image, ' ');
      else
         return "";
      end if;
   end Long_Description;

   --------------------
   -- Sort_Compilers --
   --------------------

   function Sort_Compilers (L, R : Release) return Boolean is

      -----------------
      -- Is_External --
      -----------------

      function Is_External (This : Release) return Boolean
      is (This.Name = GNAT_External_Crate);

      ---------------
      -- Is_Native --
      ---------------

      function Is_Native (This : Release) return Boolean is
      begin
         return AAA.Strings.Has_Suffix (This.Name.As_String, "_native");
         --  A lil' bit of magic to recognize the native compilers
      end Is_Native;

   begin

      --  External is preferred to any other compiler. This can be overridden
      --  by explicitly selecting a compiler with `alr toolchain --select`, or
      --  by specifying a targeted gnat_xxx compiler.

      if Is_External (L) xor Is_External (R) then
         return Is_External (R);
      end if;

      --  Native goes next in preferences (preferred to cross-compilers)

      if Is_Native (L) xor Is_Native (R) then
         return Is_Native (R);
      end if;

      --  otherwise same ordering as regular crates

      return Standard_Sorting (L, R);
   end Sort_Compilers;

end Alire.Releases;
