with AAA.Table_IO;

with Ada.Directories;

with Alire.Dependencies.Containers;
with Alire.Directories;
with Alire.Paths;
with Alire.Roots.Editable;
with Alire.Roots.Optional;
with Alire.Solutions;
with Alire.Solver.Predefined_Options;
with Alire.Spawn;
with Alire.Utils.TTY;

with CLIC.User_Input;

package body Alire.Install is

   package Adirs renames Ada.Directories;
   package TTY renames Alire.Utils.TTY;

   use Directories.Operators;
   use type AAA.Strings.Vector;

   -----------------
   -- Sync_Prefix --
   -----------------

   procedure Sync_Prefix (Prefix_Path : Absolute_Path;
                          Root        : in out Roots.Root)
   is

      ------------------
      -- Clean_Prefix --
      ------------------

      procedure Clean_Prefix is
         Status : Simple_Logging.Ongoing :=
                    Simple_Logging.Activity ("Cleaning up");

         --------------------
         -- Clean_Internal --
         --------------------

         procedure Clean_Internal (Item : Ada.Directories.Directory_Entry_Type;
                                   Stop : in out Boolean)
         is
            Target : constant String := Adirs.Full_Name (Item);
         begin
            Stop := False;

            if Adirs.Simple_Name (Item) = Paths.Root_Folder_Inside_Prefix then
               Trace.Debug ("Sparing private workspace from deletion: "
                            & Target);
               return;
            end if;

            if GNAT.OS_Lib.Is_Directory (Target) then
               Trace.Debug ("Deleting dir: " & Target);
               Directories.Delete_Tree (Target);
            else
               Trace.Debug ("Deleting file: " & Target);
               Adirs.Delete_File (Target);
            end if;

            Status.Step;
         end Clean_Internal;

      begin
         --  Remove any entry that isn't our private workspace
         Directories.Traverse_Tree (Prefix_Path,
                                    Clean_Internal'Access,
                                    Recurse => False);
      end Clean_Prefix;

   begin
      Root.Generate_Configuration;

      Assert (Root.Build (Cmd_Args         => AAA.Strings.Empty_Vector,
                          Export_Build_Env => True,
                          Build_All_Deps   => True,
                          Build_Root       => False),
              "Build failure, prefix broken");
      --  TODO: automatic rollback on build failure

      --  Clean up installation to have a fresh one
      --  TODO: incremental install/remove for speed-up

      Clean_Prefix;

      --  Final, actual gprinstallation

      Root.Install (Prefix_Path);
   end Sync_Prefix;

   ---------
   -- Add --
   ---------

   procedure Add (Prefix : Any_Path;
                  Deps   : Dependencies.Containers.List)
   is
      Prefix_Path : constant Absolute_Path := Adirs.Full_Name (Prefix);

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
         Parent : constant Absolute_Path :=
                    Adirs.Containing_Directory (Prefix_Path);
         Root_Path : constant Absolute_Path :=
                       Prefix_Path / Paths.Root_Folder_Inside_Prefix;
      begin

         if not GNAT.OS_Lib.Is_Directory (Parent) then
            Raise_Checked_Error ("Parent of given location does not exist: "
                                 & TTY.URL (Parent));
         end if;

         Adirs.Create_Path (Root_Path);

         declare
            Guard : Directories.Guard (Directories.Enter (Root_Path))
              with Unreferenced;
         begin
            Spawn.Command ("alr",
                           AAA.Strings.Empty_Vector
                           & "-q"
                           & "init"
                           & "--lib"
                           & "--in-place"
                           & "alire_install_prefix");
            --  TODO: init functionality is still entirely in Alr.Commands.Init
            --  so we can't easily initialize a buildable prefix without
            --  externally calling alr. At some point that init code should
            --  be moved into Alire.Roots.

            --  We need a first empty solution and populated config

            declare
               Root : Roots.Root := Roots.Load_Root (Root_Path);
            begin
               Root.Set (Solutions.Empty_Valid_Solution);
            end;
         end;
      end Initialize;

      ------------------------------
      -- Verify_Complete_Solution --
      ------------------------------

      procedure Verify_Complete_Solution (Root : in out Roots.Editable.Root) is
      begin
         declare
            Solution : constant Solutions.Solution :=
                         Root.Compute_Solution
                           (Options =>
                              Solver.Predefined_Options.First_Incomplete);
         begin
            if not Solution.Is_Complete then
               Put_Warning ("Solution is incomplete:");
               Trace.Always ("");
               Solution.Print_Tree (Root       => Root.Current.Release,
                                    Prefix     => "   ",
                                    Print_Root => False);
               Trace.Always ("");
               raise Query_Unsuccessful;
            end if;
         end;
      exception
         when Query_Unsuccessful =>
            Raise_Checked_Error
                 ("Could not find a complete solution for the prefix; "
                  & "installation cannot proceed.");
      end Verify_Complete_Solution;

      -----------------
      -- Add_Targets --
      -----------------

      procedure Add_Targets is
         use CLIC.User_Input;
         Root : constant Roots.Optional.Root := Install.Root (Prefix);
      begin
         if not Root.Is_Valid then
            if Query (Question =>
                        "Install prefix not found at "
                        & TTY.URL (Prefix_Path) & ", "
                        & "do you want to create one?",
                      Valid    => (Yes | No => True, others => False),
                      Default  => Yes) = Yes
            then
               Initialize;
               Add_Targets;
               return;
            else
               Raise_Checked_Error ("Cancelled.");
            end if;
         end if;

         --  Addition proper, Root is for sure valid

         declare
            Edit : Roots.Editable.Root := Roots.Editable.New_Root (Root.Value);
         begin
            for Dep of Deps loop
               Edit.Add_Dependency (Dep, Narrow_Down => False);
               --  Let the user be broad if they want
            end loop;

            Verify_Complete_Solution (Edit);
            --  We'll abort if solution isn't complete

            Edit.Confirm_And_Commit
              (Options => Solver.Predefined_Options.Complete_Only);

            Sync_Prefix (Prefix_Path, Edit.Current);
         end;

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

      Add_Targets;
   end Add;

   -----------
   -- Print --
   -----------

   procedure Print (Prefix : Any_Path) is
      Path : constant String := Adirs.Full_Name (Prefix);
      Root : constant Roots.Optional.Root := Install.Root (Path);

      --------------------
      -- Print_Internal --
      --------------------

      procedure Print_Internal (Root : in out Roots.Root) is
         Table    : AAA.Table_IO.Table;
      begin
         Trace.Always ("PATH: " & TTY.URL (Path));

         if Root.Solution.Releases.Is_Empty then
            Trace.Always ("(empty)");
            return;
         end if;

         Table.Append ("CRATE").Append ("VERSION").Append ("TYPE").New_Row;

         for Rel of Root.Solution.Releases loop
            Table.Append (TTY.Name (Rel.Name));
            Table.Append (TTY.Version (Rel.Version_Image));
            if (for some Dep of Root.Release.Flat_Dependencies =>
                  Rel.Provides (Dep.Crate))
            then
               Table.Append (CLIC.TTY.OK ("installed"));
            else
               Table.Append (CLIC.TTY.Warn ("dependency"));
            end if;
            Table.New_Row;
         end loop;

         Table.Print;
      end Print_Internal;

   begin
      if Root.Is_Valid then
         Print_Internal (Root.Value);
      else
         Put_Info ("No installation found at " & TTY.URL (Path));
         return;
      end if;
   end Print;

   ------------
   -- Remove --
   ------------

   procedure Remove (Prefix : Any_Path;
                     Crates : Containers.Crate_Name_Sets.Set)
   is
      Root : constant Roots.Optional.Root := Install.Root (Prefix);
      Prefix_Path : constant Absolute_Path := Adirs.Full_Name (Prefix);

      ---------------------
      -- Remove_Internal --
      ---------------------

      procedure Remove_Internal is
         Edit : Roots.Editable.Root := Roots.Editable.New_Root (Root.Value);
      begin

         --  First

         for Crate of Crates loop
            Edit.Remove_Dependency (Crate);
         end loop;

         Edit.Confirm_And_Commit (Options =>
                                    Solver.Predefined_Options.Complete_Only);

         Sync_Prefix (Prefix_Path, Edit.Current);
      end Remove_Internal;

   begin
      if not Root.Is_Valid then
         Raise_Checked_Error ("Install prefix not found at "
                              & TTY.URL (Prefix_Path));
      end if;

      --  Ensure all crates are there and are direct dependencies

      for Crate of Crates loop
         if not Root.Value.Release.Depends_On (Crate) then
            if Root.Value.Solution.Depends_On (Crate) then
               Raise_Checked_Error
                 ("Crate " & TTY.Name (Crate) & " is present only as a "
                  & "dependency and thus cannot be uninstalled");
            else
               Raise_Checked_Error
                 ("Crate " & TTY.Name (Crate) & " is not installed");
            end if;
         end if;
      end loop;

      Remove_Internal;

   end Remove;

   ----------
   -- Root --
   ----------

   function Root (Prefix : Any_Path) return Roots.Optional.Root is
   begin
      return Roots.Optional.Detect_Root
        (Prefix / Paths.Root_Folder_Inside_Prefix);
   end Root;

end Alire.Install;
