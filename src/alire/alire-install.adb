with Ada.Directories;

with Alire.Dependencies.Containers;
with Alire.Errors;
with Alire.Origins;
with Alire.Platforms.Current;
with Alire.Releases;
with Alire.Solver;

package body Alire.Install is

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
         Rel.Deploy (Env           => Platforms.Current.Properties,
                     Parent_Folder => Prefix,
                     Was_There     => Was_There);

         if not Rel.Project_Files (Platforms.Current.Properties,
                                   With_Path => False).Is_Empty
         then
            --  This would require using the gprinstall, trusting there are
            --  Artifacts in there. Unimplemented for now, and not dealt with
            --  in this branch anyway (once there are project files, we would
            --  install in the context of a proper Root.)
            raise Program_Error with Errors.Set
              ("Unexpected binary release with project file");
         end if;

         Put_Info ("Installing " & Rel.Milestone.TTY_Image & "...");
         Directories.Merge_Contents
           (Src                   => Prefix / Rel.Base_Folder,
            Dst                   => Prefix,
            Skip_Top_Level_Files  => True,
            Fail_On_Existing_File => not Alire.Force);

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

   ----------
   -- Info --
   ----------

   procedure Info (Prefix : Any_Path) is
   begin
      --  gprinstall stores metadata about each install in share/gpr/manifests.
      --  For binary "just-copy" installs we use a separate metadata location.

      if not Ada.Directories.Exists
        (Prefix / Gnatinstall_Metadata_Dir_In_Prefix)
        and then not Ada.Directories.Exists
          (Prefix / Alire_Metadata_Dir_In_Prefix)
      then
         Trace.Info ("There is no installation at prefix " & TTY.URL (Prefix));
      else
         Trace.Info ("Installation prefix found at " & TTY.URL (Prefix));
      end if;

   end Info;

end Alire.Install;
