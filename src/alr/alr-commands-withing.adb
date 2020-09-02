with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

with Alire.Conditional;
with Alire.Dependencies.Diffs;
with Alire.Index;
with Alire.Manifest;
with Alire.Milestones;
with Alire.Releases;
with Alire.Roots.Optional;
with Alire.Solutions;
with Alire.Solver;
with Alire.Utils.User_Input;
with Alire.Workspace;

with Alr.Commands.User_Input;
with Alr.OS_Lib;
with Alr.Platform;
with Alr.Root;
with Alr.Utils.Auto_GPR_With;

with Semantic_Versioning.Extended;

package body Alr.Commands.Withing is

   Switch_URL : constant String := "--use";

   procedure Replace_Current
     (Old_Deps,
      New_Deps     : Alire.Conditional.Dependencies;
      Old_Solution : Alire.Solutions.Solution := Root.Current.Solution);

   -------------------
   -- Auto_GPR_With --
   -------------------

   procedure Auto_GPR_With is
   begin
      for File of Root.Current.Release.Project_Files
        (Root.Current.Environment, With_Path => True)
      loop
         Utils.Auto_GPR_With.Update
           (Alire.Directories."/" (Root.Current.Path, File),
            Root.Current.GPR_Project_Files (Exclude_Root => True));
      end loop;
   end Auto_GPR_With;

   ---------
   -- Add --
   ---------

   function Add (Deps    : Alire.Conditional.Dependencies;
                 New_Dep : String)
                 return Alire.Conditional.Dependencies
   is
      use all type Alire.Conditional.Dependencies;
      Requested : constant Alire.Milestones.Allowed_Milestones :=
        Alire.Milestones.Crate_Versions (New_Dep);
   begin

      --  Check that the requested dependency exists

      if not Alire.Index.Exists (Requested.Crate) then
         Trace.Warning
           ("The requested crate does not exist in the catalog: " &
            (+Requested.Crate));
      end if;

      --  Verify the dependency is truly new

      for Dep of Alire.Conditional.Enumerate (Deps) loop
         if Dep.Crate = Requested.Crate then
            Trace.Info
              ("Not adding " & (+Requested.Crate)
               & " because " & Dep.Image & " is already a dependency");
            return Deps;
         end if;
      end loop;

      --  Merge the dependency. Completeness of the solution will be presented
      --  as a whole after all changes have been processed, in Replace_Current.

      return Deps and Alire.Conditional.New_Dependency (Requested.Crate,
                                                        Requested.Versions);
   end Add;

   ------------------
   -- Add_Softlink --
   ------------------

   procedure Add_Softlink (Dep_Spec : String;
                           Path     : String) is
      Requested : constant Alire.Milestones.Allowed_Milestones :=
                    Alire.Milestones.Crate_Versions (Dep_Spec);
      New_Dep   : constant Alire.Dependencies.Dependency :=
                    Alire.Dependencies.From_Milestones (Requested);
   begin
      declare
         use Alire;
         use type Conditional.Dependencies;
         Old_Deps     : constant Conditional.Dependencies :=
                          Root.Current.Release.Dependencies;
         Old_Solution : constant Solutions.Solution := Root.Current.Solution;
         New_Solution : constant Solutions.Solution :=
                          Old_Solution
                            .Depending_On (New_Dep)
                            .Linking (Crate => New_Dep.Crate,
                                      Path  => Path);
      begin

         --  Prevent double-add

         if Old_Solution.Depends_On (New_Dep.Crate) then
            Reportaise_Wrong_Arguments
              ("Not adding " & New_Dep.Crate.TTY_Image & " because "
               & Old_Solution.Dependency (New_Dep.Crate).TTY_Image
               & " is already a dependency");
         end if;

         --  Report crate detection at target destination

         User_Input.Report_Pinned_Crate_Detection (New_Dep.Crate,
                                                   New_Solution);

         --  If we made here there were no errors adding the dependency
         --  and storing the softlink. We can proceed to confirming the
         --  replacement.

         Replace_Current (Old_Deps     => Old_Deps,
                          New_Deps     => Old_Deps and New_Dep,
                          Old_Solution => New_Solution);
         --  We use the New_Solution with the softlink as previous solution, so
         --  the pinned directory is used by the solver.

      end;
   end Add_Softlink;

   ---------------------
   -- Detect_Softlink --
   ---------------------

   procedure Detect_Softlink (Path : String) is
      Root : constant Alire.Roots.Optional.Root :=
               Alire.Roots.Optional.Detect_Root (Path);
   begin
      if Root.Is_Valid then
         if Root.Value.Is_Stored then
            --  Add a dependency on ^(detected version) (i.e., safely
            --  upgradable)
            Add_Softlink
              (Dep_Spec => Root.Value.Release.Name_Str
               & "^" & Root.Value.Release.Version.Image,
               Path     => Path);
         else
            Reportaise_Command_Failed
              ("cannot add target: " & Root.Value.Storage_Error);
         end if;
      else
         Reportaise_Command_Failed
           ("cannot add target: crate metadata not found at " & Path
            & " (give an explicit crate name argument to use a plain"
            & " GNAT project as dependency)");
      end if;
   end Detect_Softlink;

   ---------
   -- Del --
   ---------

   function Del (Deps    : Alire.Conditional.Dependencies;
                 Old_Dep : String)
                 return Alire.Conditional.Dependencies
   is
      use all type Alire.Conditional.Dependencies;
      use all type Semantic_Versioning.Extended.Version_Set;
      Requested : constant Alire.Milestones.Allowed_Milestones :=
                    Alire.Milestones.Crate_Versions (Old_Dep);
      Found     : Boolean := False;
   begin
      if Requested.Versions /= Semantic_Versioning.Extended.Any then
         Trace.Warning
           ("Version is not used when removing dependencies: " & Old_Dep);
      end if;

      --  Iterate over actual dependencies and remove any matching the given
      return Filtered : Alire.Conditional.Dependencies do
         if Deps.Is_Iterable then
            for Dep of Deps loop
               if Dep.Is_Value and then Dep.Value.Crate /= Requested.Crate then
                  --  A regular static dependency
                  Filtered := Filtered and Dep;
               elsif not Dep.Is_Value then
                  --  Something else (dynamic expression) that we cannot manage
                  --  programmatically.
                  Filtered := Filtered and Dep;
                  Trace.Warning
                    ("Skipping unsupported conditional dependency: "
                     & Dep.Image_One_Line);
               else
                  --  Simply don't add the one we want to remove
                  Found := True;
               end if;
            end loop;
         else
            Trace.Warning ("Skipping unsupported conditional dependency: "
                           & Deps.Image_One_Line);
         end if;

         if not Found then
            Trace.Warning
              ("Crate slated for removal is not among"
               & " direct static dependencies: "
               & (+Requested.Crate));
         end if;
      end return;
   end Del;

   ---------------------
   -- Replace_Current --
   ---------------------

   procedure Replace_Current
     (Old_Deps,
      New_Deps     : Alire.Conditional.Dependencies;
      Old_Solution : Alire.Solutions.Solution := Root.Current.Solution)
   is
   begin
      Requires_Full_Index;

      --  Set, regenerate and update
      declare
         New_Root  : constant Alire.Roots.Root :=
           Alire.Roots.New_Root
             (Root.Current.Release.Replacing (Dependencies => New_Deps),
              Root.Current.Path,
              Platform.Properties);
         New_Solution : constant Alire.Solutions.Solution :=
                          Alire.Solver.Resolve (New_Deps,
                                                Platform.Properties,
                                                Old_Solution);

         Deps_Diff : constant Alire.Dependencies.Diffs.Diff :=
                       Alire.Dependencies.Diffs.Between (Old_Deps, New_Deps);
      begin

         --  Show changes to apply

         Trace.Info ("Requested changes:");
         Trace.Info ("");
         Deps_Diff.Print;

         --  Show the effects on the solution

         if not Alire.Utils.User_Input.Confirm_Solution_Changes
           (Root.Current.Solution.Changes (New_Solution),
            Changed_Only => not Alire.Detailed)
         then
            Trace.Info ("No changes applied.");
            return;
         end if;

         --  Add changes to the manifest:

         Alire.Manifest.Append (Root.Current.Crate_File,
                                Deps_Diff.Added);
         Alire.Manifest.Remove (Root.Current.Crate_File,
                                Deps_Diff.Removed);
         Trace.Detail ("Manifest updated, fetching dependencies now");

         --  And apply changes (will also generate new lockfile)

         Alire.Workspace.Deploy_Dependencies
           (Root     => New_Root,
            Solution => New_Solution);

         Auto_GPR_With;
      end;

   end Replace_Current;

   ---------
   -- Add --
   ---------

   procedure Add is
      Old_Deps : constant Alire.Conditional.Dependencies :=
                   Root.Current.Release.Dependencies;
      New_Deps : Alire.Conditional.Dependencies := Old_Deps;
      use type Alire.Conditional.Dependencies;
   begin
      for I in 1 .. Num_Arguments loop
         New_Deps := Add (New_Deps, Argument (I));
      end loop;

      if Old_Deps /= New_Deps then
         Replace_Current (Old_Deps, New_Deps);
      end if;
   end Add;

   ---------
   -- Del --
   ---------

   procedure Del is
      Old_Deps : constant Alire.Conditional.Dependencies :=
                   Root.Current.Release.Dependencies;
      New_Deps : Alire.Conditional.Dependencies := Old_Deps;
      use type Alire.Conditional.Dependencies;
   begin
      for I in 1 .. Num_Arguments loop
         New_Deps := Del (New_Deps, Argument (I));
      end loop;

      if Old_Deps /= New_Deps then
         Replace_Current (Old_Deps, New_Deps);
      else
         Trace.Warning ("There are no changes to apply.");
      end if;
   end Del;

   ----------
   -- From --
   ----------

   procedure From is
      use Ada.Text_IO;
      use Utils;

      Deps : Alire.Conditional.Dependencies;

      -------------
      -- Extract --
      -------------

      procedure Extract (Line : String) is
         use Ada.Strings;
         use Ada.Strings.Fixed;
         use Ada.Strings.Maps;
         --  Line contains "alr with", is crunched and lowercased
         First, Last : Natural := Line'First - 1;

         type Found_Steps is (Nothing, Dashes, Alr, Withh);
         Found : Found_Steps := Nothing;
      begin
         loop
            Find_Token (Line, To_Set (' '),
                        From  => Last + 1,
                        Test  => Outside,
                        First => First,
                        Last  => Last);

            exit when First > Line'Last;

            case Found is
               when Nothing =>
                  if Line (First .. Last) = "--" then
                     Found := Dashes;
                  end if;
               when Dashes =>
                  if Line (First .. Last) = "alr" then
                     Found := Alr;
                  end if;
               when Alr =>
                  if Line (First .. Last) = "with" then
                     Found := Withh;
                  end if;
               when Withh =>
                  Deps := Add (Deps, Line (First .. Last));
            end case;

            exit when Last = Line'Last;
         end loop;
      end Extract;

      ----------------
      -- Check_File --
      ----------------

      procedure Check_File (Name : String) is
         File : File_Type;
      begin
         if not OS_Lib.Is_Regular_File (Name) then
            Reportaise_Command_Failed ("Given file not found: " & Name);
         end if;

         Open (File, In_File, Name);

         while not End_Of_File (File) loop
            declare
               Line : constant String :=
                 Crunch (To_Lower_Case (Get_Line (File)));
            begin
               exit when Contains (Line, "project");

               if Contains (Line, "alr with") then
                  Extract (Line);
               end if;
            end;
         end loop;

         Close (File);
      end Check_File;
   begin
      for I in 1 .. Num_Arguments loop
         Check_File (Argument (I));
      end loop;

      if not Deps.Is_Empty then
         Replace_Current (Old_Deps => Alire.Conditional.No_Dependencies,
                          New_Deps => Deps);
      else
         Trace.Warning ("No dependencies found.");
      end if;
   end From;

   ----------
   -- List --
   ----------

   procedure List (Cmd : Command) is
      Root_Release : constant Alire.Releases.Release := Root.Current.Release;
   begin
      Put_Line ("Dependencies (direct):");
      Root_Release.Dependencies.Print ("   ",
                                       Root_Release.Dependencies.Contains_ORs,
                                       Sorted => True);

      if Cmd.Solve then
         Requires_Full_Index; -- Load possible hints
         Root.Current.Solution.Print (Root_Release,
                                      Platform.Properties,
                                      Detailed => True,
                                      Level    => Always);
      end if;
   end List;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Cmd : in out Command) is
      Flags : Natural := 0;

      procedure Check (Flag : Boolean) is
      begin
         if Flag then
            Flags := Flags + 1;
         end if;

         if Flags > 1 then
            Reportaise_Wrong_Arguments
              ("Only one simultaneous switch allowed.");
         end if;
      end Check;

   begin
      Requires_Valid_Session;

      if Cmd.URL.all /= "" then
         Flags := Flags + 1;
      end if;

      Check (Cmd.Del);
      Check (Cmd.From);
      Check (Cmd.Graph);
      Check (Cmd.Solve);
      Check (Cmd.Tree);
      Check (Cmd.Versions);

      --  No parameters: give requested info and return. There is still the
      --  possibility of a `with --use` that is processed later.

      if Num_Arguments = 0 then
         if Flags = 0 or else Cmd.Solve then
            List (Cmd);
            return;
         elsif Cmd.Tree then
            Root.Current.Solution.Print_Tree (Root.Current.Release);
            return;
         elsif Cmd.Graph then
            Root.Current.Solution.Print_Graph
              (Root.Current.Release, Platform.Properties);
            return;
         elsif Cmd.Versions then
            Requires_Full_Index;
            Root.Current.Solution.Print_Versions (Root.Current);
            return;
         end if;
      end if;

      if Num_Arguments < 1 then
         if Cmd.Del then
            Reportaise_Wrong_Arguments ("At least one dependency required");
         elsif Cmd.From then
            Reportaise_Wrong_Arguments
              ("At least one GPR file to process required");
         end if;
      end if;

      if not (Cmd.Del or else Cmd.From) then

         --  Must be Add, but it could be regular or softlink

         if Cmd.URL.all /= "" then
            if Num_Arguments = 1 then
               Add_Softlink (Dep_Spec => Argument (1),
                             Path     => Cmd.URL.all);
            else
               Detect_Softlink (Cmd.URL.all);
            end if;
         else
            Requires_Full_Index;
            Add;
         end if;

      elsif Cmd.Del then
         Del;
      elsif Cmd.From then
         Requires_Full_Index;
         From;
      else
         raise Program_Error with "List should have already happened";
      end if;
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return Alire.Utils.String_Vector
   is (Alire.Utils.Empty_Vector
       .Append ("Inspect and manage dependencies.")
       .New_Line
       .Append ("* Inspecting dependencies:")
       .Append ("Run without arguments prints current dependencies. Use"
                & " --solve to print the solution in use for these"
                & " dependencies.")
       .New_Line
       .Append ("* Adding dependencies from the command line:")
       .Append ("Dependencies are added by giving their name, and removed"
                & " by using the --del flag. Dependencies cannot be"
                & " simultaneously added and removed in a single invocation.")
       .New_Line
       .Append ("* Adding dependencies pinned to external sources:")
       .Append ("When a single crate name is accompanied by an --use PATH"
                & " argument, the crate is always fulfilled for any required"
                & " version by the sources found at PATH.")
       .New_Line
       .Append ("* Adding dependencies from a GPR file:")
       .Append ("The project file given with --from will be scanned looking"
                & " for comments that contain the sequence 'alr with'. "
                & " These will be processed individually as if they had been"
                & " given in the command line, starting with no dependencies."
                & " That is, only dependencies given in the GPR file will be"
                & " preserved.")
       .New_Line
       .Append ("Example of GPR file contents:")
       .New_Line
       .Append ("with ""libhello""; -- alr with libhello")
       .New_Line
       .Append ("* Caveat:")
       .Append ("Since alr does not modify user files, any dependencies"
                & " managed through this command only directly affect"
                & " the metadata files of alr itself. In order to use the"
                & " dependencies in Ada code, the user *must* add the needed"
                & " 'with'ed project files in their own GPR files.")
       .New_Line
       .Append (Crate_Version_Sets));

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
      use GNAT.Command_Line;
   begin
      Define_Switch (Config,
                     Cmd.Del'Access,
                     "", "--del",
                     "Remove given dependencies");

      Define_Switch (Config,
                     Cmd.From'Access,
                     "", "--from",
                     "Use dependencies declared within GPR project file");

      Define_Switch (Config,
                     Cmd.Graph'Access,
                     "", "--graph",
                     "Show ASCII graph of dependencies");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.URL'Access,
         Long_Switch => Switch_URL & "=",
         Argument    => "PATH",
         Help        => "Add a dependency pinned to some external source");

      Define_Switch (Config,
                     Cmd.Solve'Access,
                     "", "--solve",
                     "Show complete solution to dependencies");

      Define_Switch (Config,
                     Cmd.Tree'Access,
                     "", "--tree",
                     "Show complete dependency tree");

      Define_Switch (Config,
                     Cmd.Versions'Access,
                     "", "--versions",
                     "Show version status of dependencies");
   end Setup_Switches;

end Alr.Commands.Withing;
