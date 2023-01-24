with Ada.Directories;

with Alire.Dependencies.Containers;
with Alire.Errors;
with Alire.Origins;
with Alire.Platforms.Current;
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

         Set_Not_Installed (Prefix, Rel.Name);
         Set_Installed (Prefix, Rel.Milestone);

         --  Remove unwanted remains, if any

         Directories.Force_Delete (Prefix / Rel.Base_Folder);
      end Install_Binary;

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
               Check_Conflict (Prefix, Rel);
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

      Directories.Create_Tree (Prefix / Metadata_Dir_In_Prefix);
      --  Ensure destination exists

      Add_Targets;
   end Add;

   --------------------
   -- Check_Conflict --
   --------------------

   procedure Check_Conflict (Prefix : Any_Path; Rel : Releases.Release) is
      Installed : constant Installed_Milestones := Find_Installed (Prefix);
   begin
      if (for some M of Installed => M.Crate = Rel.Name) then
         if Installed.Contains (Rel.Milestone) then
            Recoverable_Error
              ("Requested release " & Rel.Milestone.TTY_Image
               & " is already installed");
         else
            Recoverable_Error
              (Errors.Wrap
                 ("Requested release " & Rel.Milestone.TTY_Image
                  & " has another version already installed: ",
                  To_Image_Vector (Find_Installed
                    (Prefix, Rel.Name)).Flatten (ASCII.LF)));
         end if;
      end if;
   end Check_Conflict;

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
