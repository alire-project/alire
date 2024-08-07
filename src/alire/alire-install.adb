with Ada.Containers;
with Ada.Directories;

with Alire.Dependencies.Containers;
with Alire.Errors;
with Alire.Index;
with Alire.Optional;
with Alire.Origins;
with Alire.Platforms.Current;
with Alire.Roots;
with Alire.Solutions.Containers;
with Alire.Solver;

package body Alire.Install is

   package Adirs renames Ada.Directories;

   ---------------------
   -- To_Image_Vector --
   ---------------------

   function To_Image_Vector (This : Installed_Milestones)
                             return AAA.Strings.Vector
   is
   begin
      return Result : AAA.Strings.Vector do
         for M of This loop
            Result.Append (M.TTY_Image);
         end loop;
      end return;
   end To_Image_Vector;

   ---------
   -- Add --
   ---------

   procedure Add (Prefix : Any_Path;
                  Deps   : Dependencies.Containers.List)
   is

      --------------------
      -- Install_Binary --
      --------------------

      procedure Install_Binary (Rel : Releases.Release) is
         Was_There : Boolean := False;
      begin

         --  Use or regular deployment facilities, in case there are any
         --  actions to perform.

         Rel.Deploy (Env             => Platforms.Current.Properties,
                     Parent_Folder   => Prefix,
                     Was_There       => Was_There,
                     Mark_Completion => False);
         --  We set Mark_Completion to False because the deployment folder
         --  is temporary, so we don't need to track completion if the
         --  installation fails, and otherwise we will have a common file to
         --  all installations (the ./alire/ canary file) that will cause a
         --  clash after the first installation.

         if not Rel.Project_Files (Platforms.Current.Properties,
                                   With_Path => False).Is_Empty
         then
            --  This would require using gprinstall, trusting there are
            --  Artifacts in the project file. Unimplemented for now, and not
            --  dealt with in this branch anyway (once there are project files,
            --  we would install in the context of a proper Root.)
            Put_Warning ("Ignoring project files for binary release "
                         & Rel.Milestone.TTY_Image);
         end if;

         --  Now move into the proper place

         Put_Info ("Installing " & Rel.Milestone.TTY_Image & "...");
         Directories.Merge_Contents
           (Src                   => Prefix / Rel.Base_Folder,
            Dst                   => Prefix,
            Skip_Top_Level_Files  => True,
            Fail_On_Existing_File => not Alire.Force,
            Remove_From_Source    => True);

         --  Keep track that this was installed

         Set_Not_Installed (Prefix, Rel.Name);
         Set_Installed (Prefix, Rel.Milestone);

         --  Remove unwanted remains, if any

         Directories.Force_Delete (Prefix / Rel.Base_Folder);
      end Install_Binary;

      ---------------------
      -- Install_Regular --
      ---------------------

      procedure Install_Regular (Rel : Releases.Release;
                                 Sol : Solutions.Solution)
      is
         use type Ada.Containers.Count_Type;
         Temp : Directories.Temp_File;
      begin
         Put_Info ("Starting deployment of "
                   & Rel.Milestone.TTY_Image
                   & " to fulfill " & Sol.State (Rel).As_Dependency.TTY_Image
                   & " with "
                   & (if Sol.All_Dependencies.Length <= 1
                     then "no dependencies."
                     else "solution:"));
         if Sol.All_Dependencies.Length > 1 then
            --  We always depend on the root release at least
            Sol.Print (Root     => Rel,
                       Env      => Platforms.Current.Properties,
                       Detailed => False,
                       Level    => Info,
                       Prefix   => "   ",
                       Graph    => False);
         end if;

         Trace.Debug ("Deploying installation target "
                      & Rel.Milestone.TTY_Image
                      & " to " & Temp.Filename);

         declare
            Root : Roots.Root := Roots.Create_For_Release
              (This          => Rel,
               Parent_Folder => Temp.Filename,
               Env           => Platforms.Current.Properties,
               Up_To         => Roots.Deploy);
         begin
            Root.Set (Sol.Excluding (Rel.Name));
            --  We exclude the root release as the Root type will take it into
            --  account and otherwise it would be as if Rel depended on itself.

            Root.Deploy_Dependencies;
            Root.Install (Prefix, Print_Solution => False);
         end;
      end Install_Regular;

      -----------------------
      -- Compute_Solutions --
      -----------------------
      --  Look for all solutions at once. This way, if some crate is unsolvable
      --  we fail early on before fetching/compiling anything.
      function Compute_Solutions return Solutions.Containers.Map is
         use all type Origins.Kinds;
         Result : Solutions.Containers.Map;

         --------------------
         -- Compute_Binary --
         --------------------
         --  Look for a binary crate for the dependency. For installation we
         --  always prefer a binary release over a source one. The user can
         --  always override by giving specific versions if there were of
         --  both kinds for the same crate.
         function Compute_Binary (Dep : Dependencies.Dependency)
                                  return Boolean
         is
         begin
            declare
               Rel : constant Releases.Release
                 := Solver.Find (Name    => Dep.Crate,
                                 Allowed => Dep.Versions,
                                 Policy  => Solver.Newest,
                                 Origins => (Binary_Archive => True,
                                             others         => False));
            begin
               Result.Insert (Dep.Crate,
                              Solutions
                              .Empty_Valid_Solution
                              .Depending_On (Dep)
                              .Including
                                (Rel,
                                 Env => Platforms.Current.Properties,
                                 For_Dependency =>
                                   Optional.Crate_Names.Unit (Dep.Crate)));
               return True;
            end;
         exception
            when Query_Unsuccessful =>
               Trace.Debug ("No binary release found for " & Dep.TTY_Image);
               return False;
         end Compute_Binary;

         ---------------------
         -- Compute_Regular --
         ---------------------
         --  Look for a regular solution to a dependency as fallback if we
         --  didn't find any binary solution.
         procedure Compute_Regular (Dep : Dependencies.Dependency) is
            Sol : constant Solutions.Solution := Solver.Resolve (Dep);
         begin
            if Sol.Is_Complete then
               Result.Insert (Dep.Crate, Sol);
            else
               Trace.Error ("Could not find a complete solution for "
                            & Dep.TTY_Image);

               --  If we found a release for the root dependency we can print
               --  the partial solution. Otherwise nothing was solved.

               if Sol.Contains_Release (Dep.Crate) then
                  Trace.Error ("Best incomplete solution is:");
                  Sol.Print (Root     => Sol.Releases.Element (Dep.Crate),
                             Env      => Platforms.Current.Properties,
                             Detailed => False,
                             Level    => Trace.Error);
                  Raise_Checked_Error ("Installation cannot continue.");
               elsif not Index.Exists (Dep.Crate) then
                  Raise_Checked_Error
                    ("Requested crate not found: " & Dep.Crate.TTY_Image);
               else
                  Raise_Checked_Error
                    ("No solution could be computed for: " & Dep.TTY_Image);
               end if;
            end if;
         end Compute_Regular;

      begin
         Put_Info ("Computing solutions...");

         for Dep of Deps loop
            if not Compute_Binary (Dep) then
               Compute_Regular (Dep);
            end if;
         end loop;

         Put_Success ("Installation targets fully solved");

         return Result;
      end Compute_Solutions;

      -----------------
      -- Add_Targets --
      -----------------

      procedure Add_Targets is
         --  Try to get complete solutions for everything before starting
         --  deploying anything. This way, if something isn't installable,
         --  we fail early on before fetching/compiling things.
         Sols : constant Solutions.Containers.Map := Compute_Solutions;
      begin

         --  Now install all solutions

         for I in Sols.Iterate loop
            declare
               use all type Origins.Kinds;
               use Solutions.Containers.Maps;
               Rel : constant Releases.Release :=
                       Element (I).Releases.Element (Key (I));
               Action : constant Actions := Check_Conflicts (Prefix, Rel);
            begin
               if Action = Skip then
                  Put_Info ("Skipping already installed "
                            & Rel.Milestone.TTY_Image);
               else
                  case Rel.Origin.Kind is
                     when Filesystem | Source_Archive | Origins.VCS_Kinds =>
                        Install_Regular (Rel, Sols (I));
                     when Binary_Archive =>
                        Install_Binary (Rel);
                     when others =>
                        Raise_Checked_Error
                          ("Cannot install " & Rel.Milestone.TTY_Image
                           & " because origin is of kind "
                           & Rel.Origin.Kind'Image);
                  end case;
               end if;
            end;
         end loop;
      end Add_Targets;

      Target_Deps : Dependencies.Containers.Map;

   begin
      --  Ensure no duplicates

      for Dep of Deps loop
         if Target_Deps.Contains (Dep.Crate) then
            Raise_Checked_Error ("Crate given twice for simultaneous install: "
                                 & Target_Deps (Dep.Crate).TTY_Image & " and "
                                 & Dep.TTY_Image);
         else
            Target_Deps.Insert (Dep.Crate, Dep);
         end if;
      end loop;

      Directories.Create_Tree (Prefix / Metadata_Dir_In_Prefix);
      --  Ensure destination exists

      Add_Targets;
   end Add;

   ---------------------
   -- Check_Conflicts --
   ---------------------

   function Check_Conflicts (Prefix : Any_Path;
                             Rel    : Releases.Release)
                             return Actions
   is
   begin

      --  Crates declaring executables can only be installed once

      if Rel.Origin.Kind in Origins.Binary_Archive
        or else not Rel.Executables.Is_Empty
      then
         declare
            Installed : constant Alire.Install.Installed_Milestones :=
                          Alire.Install.Find_Installed
                            (Prefix, Rel.Name);
         begin

            --  No problem if the version installed is the same one

            if Installed.Contains (Rel.Milestone) then
               return Action : constant Actions :=
                 (if Force then Reinstall else Skip)
               do
                  Trace.Debug ("Already installed: " & Rel.Milestone.TTY_Image
                               & "; action: " & Action'Image);
               end return;

            elsif not Installed.Is_Empty then

               --  A different version exists, here we fail unless forced

               Recoverable_User_Error
                 (Errors.New_Wrapper
                    ("Release " & Rel.Milestone.TTY_Image
                     & " has another version already installed: ")
                  .Wrap (To_Image_Vector (Find_Installed
                    (Prefix, Rel.Name)).Flatten (ASCII.LF))
                  .Wrap ("Releases installing executables can be "
                    & "installed only once")
                  .Wrap ("Forcing this install will overwrite the "
                    & "release already installed")
                  .Get);

               return Replace;

            end if;
         end;
      else

         --  This is a library, several versions are OK but we can skip one
         --  already available. Or it could be a crate with default undeclared
         --  executable. In any case, we cannot be sure. Worst case, gprinstall
         --  will fail later.

         if Alire.Install.Find_Installed (Prefix).Contains (Rel.Milestone)
         then
            return Action : constant Actions :=
              (if Force then Reinstall else Skip)
            do
               Trace.Debug ("Already installed: " & Rel.Milestone.TTY_Image
                            & "; action: " & Action'Image);
            end return;
         end if;

      end if;

      --  In any other case it shuld be safe to install

      return New_Install;

   end Check_Conflicts;

   --------------------
   -- Find_Installed --
   --------------------

   function Find_Installed (Prefix : Any_Path;
                            Crate  : Crate_Name)
                            return Installed_Milestones is
   begin
      return Result : Installed_Milestones do
         for M of Find_Installed (Prefix) loop
            if M.Crate = Crate then
               Result.Include (M);
            end if;
         end loop;
      end return;
   end Find_Installed;

   --------------------
   -- Find_Installed --
   --------------------

   function Find_Installed (Prefix : Any_Path)
                            return Installed_Milestones
   is
      Result : Installed_Milestones;

      procedure Find
        (Item : Any_Path;
         Stop : in out Boolean)
      is
         Name : constant String := Adirs.Simple_Name (Item);
      begin
         Stop := False;

         if (for some Char of Name => Char = '=') then
            declare
               Milestone : constant Milestones.Milestone :=
                             Milestones.New_Milestone (Name);
            begin
               Result.Insert (Milestone);
            end;
         end if;
      end Find;

   begin
      if Adirs.Exists (Prefix / Metadata_Dir_In_Prefix) then
         Directories.Traverse_Tree (Start   => Prefix / Metadata_Dir_In_Prefix,
                                    Doing   => Find'Access,
                                    Recurse => False);
      end if;

      return Result;
   end Find_Installed;

   ----------
   -- Info --
   ----------

   procedure Info (Prefix : Any_Path) is
   begin
      --  gprinstall stores metadata about each install in share/gpr/manifests.
      --  For binary "just-copy" installs we just track the milestone.

      if not Ada.Directories.Exists (Prefix / Metadata_Dir_In_Prefix) then
         Trace.Info ("There is no installation at prefix " & TTY.URL (Prefix));
      else
         Trace.Info ("Installation prefix found at " & TTY.URL (Prefix));
         Trace.Info ("Contents:");
         for Milestone of Find_Installed (Prefix) loop
            Trace.Info ("   " & Milestone.TTY_Image);
         end loop;
      end if;

   end Info;

   -------------------
   -- Set_Installed --
   -------------------

   procedure Set_Installed (Prefix : Any_Path; Mil : Milestones.Milestone) is
   begin
      Directories.Touch (Prefix
                         / Metadata_Dir_In_Prefix
                         / Mil.Image);
   end Set_Installed;

   -----------------------
   -- Set_Not_Installed --
   -----------------------

   procedure Set_Not_Installed (Prefix : Any_Path; Crate : Crate_Name) is
   begin
      for Mil of Find_Installed (Prefix) loop
         if Mil.Crate = Crate then
            Trace.Detail ("Deleting installation metadata file: "
                            & (Prefix / Metadata_Dir_In_Prefix / Mil.Image));
            Adirs.Delete_File (Prefix / Metadata_Dir_In_Prefix / Mil.Image);
         end if;
      end loop;
   end Set_Not_Installed;

end Alire.Install;
