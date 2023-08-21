with Alire.Conditional;
with Alire.Dependencies.Diffs;
with Alire.Directories;
with Alire.Manifest;
with Alire.Origins;
with Alire.Roots.Optional;
with Alire.User_Pins;
with Alire.Utils.User_Input;
with Alire.VCSs.Git;

with Semantic_Versioning.Extended;

package body Alire.Roots.Editable is

   package Semver renames Semantic_Versioning;

   --------------
   -- New_Root --
   --------------

   function New_Root (Original : in out Roots.Root) return Root
   is
   begin
      return Result : Root do
         Result.Orig := Original;
         Result.Edit := Roots.Root (Original.Temporary_Copy);
         Result.Edit.Sync_Pins_From_Manifest (Exhaustive => False);
      end return;
   end New_Root;

   ------------------------
   -- Confirm_And_Commit --
   ------------------------

   procedure Confirm_And_Commit (This : in out Root) is
      Original : Roots.Root renames This.Orig;
      Edited   : Roots.Root renames This.Edit;
   begin
      declare
         Dep_Diff : constant Dependencies.Diffs.Diff :=
                      Dependencies.Diffs.Between
                        (Former => Release (Original)
                         .Dependencies (Original.Environment),
                         Latter => Release (Edited)
                         .Dependencies (Edited.Environment));
      begin

         --  First show requested changes

         if Dep_Diff.Contains_Changes then
            Trace.Info ("Requested changes:");
            Trace.Info ("");
            Dep_Diff.Print;
         end if;

         --  Compute the new solution

         Edited.Set (Solution => Edited.Compute_Update);

         --  Then show the effects on the solution

         if Alire.Utils.User_Input.Confirm_Solution_Changes
           (Original.Solution.Changes (Edited.Solution),
            Changed_Only => not Alire.Detailed)
         then
            Edited.Commit;
            Edited.Deploy_Dependencies;
         else
            Trace.Info ("No changes applied.");
         end if;
      end;
   end Confirm_And_Commit;

   --------------------
   -- Add_Dependency --
   --------------------

   procedure Add_Dependency (This : in out Root;
                             Dep  : Dependencies.Dependency)
   is

      --------------------
      -- Find_Updatable --
      --------------------

      function Find_Updatable return Dependencies.Dependency is
      begin

         --  Solve with the new dependency and take the updatable set for the
         --  version in the solution.

         Trace.Debug ("Attempting to narrow down dependency for new crate "
                      & Dep.TTY_Image);

         declare
            use type Conditional.For_Dependencies.Tree;
            Sol : constant Solutions.Solution :=
                    Solver.Resolve
                      (Deps    => Release (This.Edit)
                                  .Dependencies (This.Edit.Environment)
                                  and Dep,
                       Props   => This.Edit.Environment,
                       Pins    => This.Edit.Pins);
         begin
            if Sol.State (Dep.Crate).Has_Release then
               return
                 Dependencies.New_Dependency
                   (Crate    => Dep.Crate,
                    Versions => Semver.Updatable
                      (Sol.State (Dep.Crate).Release.Version));
            else
               return Dep;
            end if;

         exception
            when Query_Unsuccessful =>
               Put_Warning ("No solution found when adding dependency: "
                            & Dep.TTY_Image);
               return Dep;
         end;
      end Find_Updatable;

   begin

      --  Do not add if already a direct dependency

      if Release (This.Edit).Depends_On (Dep.Crate, This.Edit.Environment) then
         raise Checked_Error with Errors.Set
           (Utils.TTY.Name (Dep.Crate) & " is already a direct dependency.");
      end if;

      --  If we are given an Any dependency, attempt a solving to narrow down
      --  to a "safely updatable" subset.

      declare
         Dep : constant Dependencies.Dependency :=
                 (if Add_Dependency.Dep.Versions.Is_Any
                  then Find_Updatable
                  else Add_Dependency.Dep);
      begin
         Alire.Manifest.Append (Crate_File (This.Edit), Dep);
         This.Reload_Manifest;

         This.Edit.Set (This.Solution.Missing (Dep,
                                               Dependencies.States.Skipped));
      end;
   end Add_Dependency;

   -----------------------
   -- Remove_Dependency --
   -----------------------

   procedure Remove_Dependency (This  : in out Root;
                                Crate : Crate_Name;
                                Unpin : Boolean := True)
   is
   begin

      --  If dependency is not among dependencies at all, nothing to do

      if not Release (This.Edit).Depends_On (Crate) then
         Raise_Checked_Error
           ("Requested crate is not among direct dependencies.");
      end if;

      --  If dependency is not among the top-level direct dependencies, this is
      --  a dynamic dependency

      if not Release (This.Edit).Dependencies.Is_Iterable
        or else
          not (for some Dep of
                 Release (This.Edit).Dependencies =>
                     Dep.Is_Value and then Dep.Value.Crate = Crate)
      then
         Raise_Checked_Error
           ("Crate slated for removal is not among direct static dependencies:"
            & " " & Utils.TTY.Name (Crate)
            & "; please remove manually from manifest.");
      end if;

      Alire.Manifest.Remove (This.Edit.Crate_File, Crate);
      This.Reload_Manifest;

      if Unpin then
         This.Remove_Pin (Crate);
      end if;

   end Remove_Dependency;

   ---------------------
   -- Add_Version_Pin --
   ---------------------

   procedure Add_Version_Pin (This    : in out Root;
                              Crate   : Crate_Name;
                              Version : Semver.Version)
   is
   begin

      --  If nothing in the solution depends on the pinned crate, add it as a
      --  direct dependency.

      if not This.Solution.Depends_On (Crate) then
         This.Add_Dependency
           (Dependencies.New_Dependency
              (Crate,
               Semver.Updatable (Version)));
      end if;

      --  Remove any previous pin for this crate

      This.Remove_Pin (Crate);

      --  And add the new pin

      Alire.Manifest.Append (Crate_File (This.Edit),
                             Crate,
                             User_Pins.New_Version (Version));
      This.Reload_Manifest;

      This.Edit.Set (This.Solution.Resetting (Crate).Pinning (Crate, Version));
   end Add_Version_Pin;

   --------------------------
   -- Add_Pin_Preparations --
   --------------------------

   function Add_Pin_Preparations (This  : in out Root;
                                  Crate : Alire.Optional.Crate_Name;
                                  Path  : Any_Path)
                                  return Crate_Name
   is
      Pin_Root : constant Optional.Root := Optional.Detect_Root (Path);

      -------------------------
      -- Pin_Is_Parent_Crate --
      -------------------------

      function Pin_Is_Parent_Crate return Boolean
      is (Directories.Find_Relative_Path_To (Path) = "..");

   begin
      --  When adding a pin from a folder other than the root, notify about it.
      --  It's likely that the user is confused about what is going on.
      if Directories.Current /= +This.Orig.Path then
         Put_Warning ("Adding pin to " & TTY.URL (Path)
                      & " in crate " & Utils.TTY.Name (This.Orig.Name)
                      & " rooted at " & TTY.URL (+This.Orig.Path));
      end if;

      if Crate.Is_Empty and then not Pin_Root.Is_Valid then
         Raise_Checked_Error
           ("No crate name given and link target is not an Alire crate:"
            & ASCII.LF & " Please provide an explicit crate name.");
      end if;

      --  No need to check that Pin_Root.Name and Crate agree, as this will be
      --  done by the pin loader.

      declare
         --  At this point we can be sure of the crate name, so we shadow the
         --  original argument.
         Crate : constant Crate_Name :=
                   (if Add_Pin_Preparations.Crate.Has_Element
                    then Add_Pin_Preparations.Crate.Element.Ptr.all
                    else Pin_Root.Value.Name);
      begin

         --  If nothing in the solution depends on the crate (that is why we
         --  check the Solution and not the top-level dependencies) requested
         --  to be pinned, we assume a top-level dependency on the crate would
         --  be wanted, and add it too. If this is not wanted, the user can
         --  easily remove the dependency by hand afterwards (or add it, if the
         --  dependency is in the closure but not in the root crate).

         --  As a special case, when the pin is a direct parent, we presume we
         --  are pinning to the main crate from a test/demo subcrate. In that
         --  case, we don't want to narrow down the version. This is merely
         --  aesthetic, as pins will always override the version.

         if not This.Solution.Depends_On (Crate) then
            This.Add_Dependency
              (Dependencies.New_Dependency
                 (Crate,
                  (if Pin_Root.Is_Valid and then not Pin_Is_Parent_Crate
                   then Pin_Root.Updatable_Dependency.Versions
                   else Semver.Extended.Any)));
         end if;

         --  Remove any previous pin for this crate

         if Release (This.Edit).Pins.Contains (Crate) then
            This.Remove_Pin (Crate);
         end if;

         return Crate;
      end;
   end Add_Pin_Preparations;

   ------------------
   -- Add_Path_Pin --
   ------------------

   procedure Add_Path_Pin (This  : in out Root;
                           Crate : Alire.Optional.Crate_Name;
                           Path  : Any_Path)
   is
      Abs_Path : constant Absolute_Path := Ada.Directories.Full_Name (Path);
      Added    : constant Crate_Name :=
                   Add_Pin_Preparations (This, Crate, Abs_Path);
      New_Pin  : constant User_Pins.Pin := User_Pins.New_Path (Abs_Path);
   begin
      --  And add the new pin

      Alire.Manifest.Append
        (Crate_File (This.Edit),
         Added,
         New_Pin);
      This.Reload_Manifest;

      This.Edit.Set (This.Solution.Linking (Added, New_Pin));

      --  Since link pins can bring in more dependencies, we must also Update.
      --  Changes will be shown afterwards on the call to Confirm_And_Commit.

      This.Edit.Update (Allow_All_Crates,
                        Silent   => True,
                        Interact => False);
   end Add_Path_Pin;

   --------------------
   -- Add_Remote_Pin --
   --------------------

   procedure Add_Remote_Pin (This   : in out Root;
                             Crate  : Alire.Optional.Crate_Name;
                             Origin : URL;
                             Ref    : String := "";
                             Branch : String := "")
   is

      ---------------------------
      -- Convert_Ref_To_Commit --
      ---------------------------

      procedure Convert_Ref_To_Commit is
         Ref    : constant String := Add_Remote_Pin.Ref;
         Commit : constant String :=
                    VCSs.Git.Handler.Remote_Commit (Origin, Ref);
      begin
         if Commit /= "" then
            Put_Info ("Using commit " & TTY.Emph (Commit)
                      & " for reference " & TTY.Emph (Ref));
            This.Add_Remote_Pin (Crate, Origin, Commit, Branch);
         else
            Raise_Checked_Error
              ("Requested remote reference " & TTY.Emph (Ref)
               & " not found in repository " & TTY.URL (Origin));
         end if;
      end Convert_Ref_To_Commit;

      Temp_Pin : Directories.Temp_File;
      --  We'll need to fetch the remote to a temporary location to verify
      --  crate matches. If all goes well, we will keep the download so there
      --  is no need to redownload on next run.
   begin

      --  We accept any reference that can be converted to a commit, as commit.
      --  This is a bit of a misnomer really in the command-line interface.

      if Ref /= "" and then not Origins.Is_Valid_Commit (Ref) then
         Convert_Ref_To_Commit;
         return;
      end if;

      --  Clone the remote so we can identify the crate and perform other
      --  validity checks.

      if not VCSs.Git.Handler.Clone
               (From   => Origin & (if Ref /= ""
                                       then "#" & Ref
                                       else ""),
                Into   => Temp_Pin.Filename,
                Branch => Branch, -- May be empty for default branch
                Depth  => 1).Success
      then
         Raise_Checked_Error
           ("Checkout of repository at " & TTY.URL (Origin)
            & " failed, re-run with -vv -d for details");
      end if;

      --  We can proceed as if it where a local pin now

      declare
         Crate : constant Crate_Name :=
                   Add_Pin_Preparations (This,
                                         Add_Remote_Pin.Crate,
                                         Temp_Pin.Filename);
         New_Pin : User_Pins.Pin :=
                     User_Pins.New_Remote (URL    => Origin,
                                           Commit => Ref,
                                           Branch => Branch);

         Destination : constant Absolute_Path :=
                         New_Pin.Deploy_Path (Crate, This.Edit.Pins_Dir);

         package Adirs renames Ada.Directories;
      begin

         --  Put in place the checkout, as it is valid if we reached this point

         if not Adirs.Exists (This.Edit.Pins_Dir) then
            Adirs.Create_Path (This.Edit.Pins_Dir);
         end if;

         if Adirs.Exists (Destination) then
            --  Remove a previous pin deployment, which may be obsolete
            Directories.Delete_Tree (Destination);
         end if;

         Adirs.Rename (Old_Name => Temp_Pin.Filename,
                       New_Name => Destination);

         --  Finally add the new pin to the manifest

         Alire.Manifest.Append (Crate_File (This.Edit),
                                Crate,
                                User_Pins.New_Remote (URL    => Origin,
                                                      Commit => Ref,
                                                      Branch => Branch));
         This.Reload_Manifest;

         --  And update lockfile. We need to call Deploy on the pin (although
         --  it is already deployed) so the pin becomes aware of its own path.

         New_Pin.Deploy (Crate, This.Edit.Pins_Dir, Online => False);
         This.Edit.Set (This.Solution.Linking (Crate, New_Pin));

         --  Since link pins can bring in more dependencies, we must
         --  also Update. Changes will be shown afterwards on the call
         --  to Confirm_And_Commit.

         This.Edit.Update (Allow_All_Crates,
                           Silent   => True,
                           Interact => False);
      end;
   end Add_Remote_Pin;

   ----------------
   -- Remove_Pin --
   ----------------

   procedure Remove_Pin (This : in out Root; Crate : Crate_Name)
   is
   begin
      if Release (This.Edit).Pins.Contains (Crate) then
         Alire.Manifest.Remove_Pin (This.Edit.Crate_File,
                                    Crate);
         This.Edit.Set (This.Solution.User_Unpinning (Crate));
         This.Reload_Manifest;
      end if;
   end Remove_Pin;

   ---------------------
   -- Reload_Manifest --
   ---------------------

   procedure Reload_Manifest (This : in out Root) is
   begin
      This.Edit.Reload_Manifest;
   end Reload_Manifest;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Root) is

      procedure Finalize (File : String) is
      begin
         if File /= "" then
            declare
               Temp : Directories.Temp_File := Directories.With_Name (File)
                 with Unreferenced;
            begin
               Trace.Debug ("Discarding temporary root file: " & File);
            end;
         end if;
      end Finalize;

   begin
      Finalize (+This.Edit.Manifest);
      Finalize (+This.Edit.Lockfile);
   exception
      when E : others =>
         Log_Exception (E, Warning);
   end Finalize;

   ---------
   -- Set --
   ---------

   procedure Set (This : in out Root; Solution : Solutions.Solution) is
   begin
      This.Edit.Set (Solution);
   end Set;

end Alire.Roots.Editable;
