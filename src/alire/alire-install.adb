with Ada.Directories;

with Alire.Dependencies.Containers;
with Alire.Errors;
with Alire.Origins;
with Alire.Platforms.Current;
with Alire.Releases;
with Alire.Solver;

with Semantic_Versioning;

package body Alire.Install is

   package Adirs renames Ada.Directories;

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

         Rel.Deploy (Env           => Platforms.Current.Properties,
                     Parent_Folder => Prefix,
                     Was_There     => Was_There);

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
            Fail_On_Existing_File => not Alire.Force);

         --  Keep track that this was installed

         Directories.Touch (Prefix
                            / Metadata_Dir_In_Prefix
                            / Rel.Milestone.Image);

         --  Remove unwanted remains, if any

         Directories.Force_Delete (Prefix / Rel.Base_Folder);
      end Install_Binary;

      --------------------
      -- Check_Conflict --
      --------------------

      procedure Check_Conflict (Rel : Releases.Release) is
         use type Semantic_Versioning.Version;
         Installed : constant Installed_Milestones := Find_Installed (Prefix);
      begin
         if Installed.Contains (Rel.Name) then
            if Installed (Rel.Name).Version = Rel.Version then
               Recoverable_Error
                 ("Requested release " & Rel.Milestone.TTY_Image
                  & " is already installed");
            else
               Recoverable_Error
                 ("Requested release " & Rel.Milestone.TTY_Image
                  & " has another version already installed: "
                  & TTY.Version (Installed (Rel.Name).Version.Image));
            end if;
         end if;
      end Check_Conflict;

      -----------------
      -- Add_Targets --
      -----------------

      procedure Add_Targets is
         use all type Origins.Kinds;
      begin
         for Dep of Deps loop
            declare
               Rel : constant Releases.Release
                 := Solver.Find (Name    => Dep.Crate,
                                 Allowed => Dep.Versions,
                                 Policy  => Solver.Newest,
                                 Origins => (Binary_Archive => True,
                                             others         => False));
            begin
               Check_Conflict (Rel);
               Install_Binary (Rel);
            end;
         end loop;
      exception
         when E : Query_Unsuccessful =>
            Errors.New_Wrapper
              .Wrap (E)
              .Wrap ("Either the release does not exist or it does not "
                     & "have a binary archive for installation.")
              .Wrap ("Only binary releases are currently supported.")
              .Print;
            Raise_Checked_Error ("Cannot complete installation.");
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

      Directories.Create_Tree (Prefix);
      --  Ensure destination exists

      Add_Targets;
   end Add;

   --------------------
   -- Find_Installed --
   --------------------

   function Find_Installed (Prefix : Any_Path)
                            return Milestones.Containers.Maps.Map
   is
      Result : Milestones.Containers.Maps.Map;

      procedure Find
        (Item : Ada.Directories.Directory_Entry_Type;
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
               Result.Insert (Milestone.Crate, Milestone);
            end;
         end if;
      end Find;

   begin
      Directories.Traverse_Tree (Start   => Prefix / Metadata_Dir_In_Prefix,
                                 Doing   => Find'Access,
                                 Recurse => False);
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

end Alire.Install;
