with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

with Alire.Conditional;
with Alire.Dependencies.Diffs;
with Alire.Index;
with Alire.Manifest;
with Alire.OS_Lib;
with Alire.Releases;
with Alire.Roots.Optional;
with Alire.Solutions;
with Alire.Solver;
with Alire.URI;
with Alire.Utils.User_Input;

with Alr.Commands.User_Input;
with Alr.OS_Lib;
with Alr.Platform;

with Semantic_Versioning.Extended;

package body Alr.Commands.Withing is

   package Semver renames Semantic_Versioning;

   Switch_URL : constant String := "--use";

   procedure Replace_Current
     (Cmd          : in out Command;
      Old_Deps,
      New_Deps     : Alire.Conditional.Dependencies;
      Old_Solution : Alire.Solutions.Solution);

   ---------
   -- Add --
   ---------

   function Add (Deps    : Alire.Conditional.Dependencies;
                 New_Dep : String)
                 return Alire.Conditional.Dependencies
   is
      use all type Alire.Conditional.Dependencies;
      Requested : constant Alire.Dependencies.Dependency :=
        Alire.Dependencies.From_String (New_Dep);
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

   ---------------------
   -- Add_Remote_Link --
   ---------------------

   procedure Add_Remote_Link (Cmd : in out Command;
                              Dep : String)
   is
      use Alire;
      Old_Deps     : constant Conditional.Dependencies :=
                       Cmd.Root.Release.Dependencies;
      New_Dep      : constant Alire.Conditional.Dependencies :=
                       (if Dep = ""
                        then Alire.Conditional.No_Dependencies
                        else Alire.Conditional.New_Dependency
                          (Alire.Dependencies.From_String (Dep)));
      New_Solution : constant Roots.Remote_Pin_Result :=
                       Cmd.Root.Pinned_To_Remote
                         (Dependency  => New_Dep,
                          URL         => Cmd.URL.all,
                          Commit      => Cmd.Commit.all,
                          Must_Depend => False);
      use type Conditional.Dependencies;
   begin

      --  Report crate detection at target destination

      User_Input.Report_Pinned_Crate_Detection (+New_Solution.Crate,
                                                New_Solution.Solution);

      --  If we made here there were no errors adding the dependency
      --  and storing the softlink. We can proceed to confirming the
      --  replacement.

      Replace_Current (Cmd,
                       Old_Deps     => Old_Deps,
                       New_Deps     => Old_Deps and New_Solution.New_Dep,
                       Old_Solution => New_Solution.Solution);
      --  We use the New_Solution with the softlink as previous solution, so
      --  the pinned directory is used by the solver.
   end Add_Remote_Link;

   ------------------
   -- Add_Softlink --
   ------------------

   procedure Add_Softlink (Cmd      : in out Command;
                           Dep_Spec : String;
                           Path     : String) is
      New_Dep : constant Alire.Dependencies.Dependency :=
                  Alire.Dependencies.From_String (Dep_Spec);
   begin
      --  Confirm target dir

      if not Alire.Utils.User_Input.Approve_Dir (Cmd.URL.all) then
         Trace.Info ("Abandoned by user.");
         return;
      end if;

      --  Prepare new solution

      declare
         use Alire;
         use type Conditional.Dependencies;
         Old_Deps     : constant Conditional.Dependencies :=
                          Cmd.Root.Release.Dependencies;
         Old_Solution : constant Solutions.Solution := Cmd.Root.Solution;
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

         Replace_Current (Cmd,
                          Old_Deps     => Old_Deps,
                          New_Deps     => Old_Deps and New_Dep,
                          Old_Solution => New_Solution);
         --  We use the New_Solution with the softlink as previous solution, so
         --  the pinned directory is used by the solver.

      end;
   end Add_Softlink;

   ---------------------
   -- Detect_Softlink --
   ---------------------

   procedure Detect_Softlink (Cmd : in out Command; Path : String) is
      Root : constant Alire.Roots.Optional.Root :=
               Alire.Roots.Optional.Detect_Root (Path);
      use all type Semver.Point;
   begin
      if Root.Is_Valid then
         if Root.Value.Is_Stored then
            --  Add a dependency on ^(detected version) (i.e., safely
            --  upgradable) or ~(detected version) (if pre-1.0).
            Add_Softlink
              (Cmd,
               Dep_Spec => Root.Value.Release.Name_Str
               & (if Semver.Major (Root.Value.Release.Version) = 0
                  then "~"
                  else "^")
               & Root.Value.Release.Version.Image,
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
      Requested : constant Alire.Dependencies.Dependency :=
                    Alire.Dependencies.From_String (Old_Dep);
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
     (Cmd          : in out Command;
      Old_Deps,
      New_Deps     : Alire.Conditional.Dependencies;
      Old_Solution : Alire.Solutions.Solution)
   is
   begin
      Cmd.Requires_Full_Index;

      --  Set, regenerate and update
      declare
         New_Root  : constant Alire.Roots.Root :=
           Alire.Roots.New_Root
             (Cmd.Root.Release.Replacing (Dependencies => New_Deps),
              Cmd.Root.Path,
              Platform.Properties);
         New_Solution : constant Alire.Solutions.Solution :=
                          Alire.Solver.Resolve
                            (New_Deps,
                             Platform.Properties,
                             Old_Solution,
                             Options => (Age    => Query_Policy,
                                         others => <>));

         Deps_Diff : constant Alire.Dependencies.Diffs.Diff :=
                       Alire.Dependencies.Diffs.Between
                         (Old_Deps,
                          Alire.Solutions.Restrict_New_Dependencies
                            (Old_Deps,
                             New_Deps,
                             New_Solution));

         use Alire.Utils.User_Input;
      begin

         --  First of all, warn about dubious caret

         if New_Root.Release.Check_Caret_Warning and then
           Query
             (Question => "Do you want to continue with that dependency?",
              Valid    => (Yes | No => True, others => False),
              Default  => No) = No
         then
            Reportaise_Command_Failed ("Abandoned by user");
         end if;

         --  Show changes to apply

         Trace.Info ("Requested changes:");
         Trace.Info ("");
         Deps_Diff.Print;

         --  Show the effects on the solution

         if not Alire.Utils.User_Input.Confirm_Solution_Changes
           (Cmd.Root.Solution.Changes (New_Solution),
            Changed_Only => not Alire.Detailed)
         then
            Trace.Info ("No changes applied.");
            return;
         end if;

         --  Add changes to the manifest:

         Alire.Manifest.Append (Cmd.Root.Crate_File,
                                Deps_Diff.Added);
         Alire.Manifest.Remove (Cmd.Root.Crate_File,
                                Deps_Diff.Removed);
         Trace.Detail ("Manifest updated, fetching dependencies now");

         --  And apply changes (will also generate new lockfile)

         Cmd.Set (New_Root);
         Cmd.Root.Set (Solution => New_Solution);
         Cmd.Root.Deploy_Dependencies;
      end;

   end Replace_Current;

   ---------
   -- Add --
   ---------

   procedure Add (Cmd : in out Command) is
      Old_Deps : constant Alire.Conditional.Dependencies :=
                   Cmd.Root.Release.Dependencies;
      New_Deps : Alire.Conditional.Dependencies := Old_Deps;
      use type Alire.Conditional.Dependencies;
   begin
      for I in 1 .. Num_Arguments loop
         New_Deps := Add (New_Deps, Argument (I));
      end loop;

      if Old_Deps /= New_Deps then
         Cmd.Replace_Current (Old_Deps, New_Deps, Cmd.Root.Solution);
      end if;
   end Add;

   ---------
   -- Del --
   ---------

   procedure Del (Cmd : in out Command) is
      Old_Deps : constant Alire.Conditional.Dependencies :=
                   Cmd.Root.Release.Dependencies;
      New_Deps : Alire.Conditional.Dependencies := Old_Deps;
      use type Alire.Conditional.Dependencies;
   begin
      for I in 1 .. Num_Arguments loop
         New_Deps := Del (New_Deps, Argument (I));
      end loop;

      if Old_Deps /= New_Deps then
         Cmd.Replace_Current (Old_Deps, New_Deps, Cmd.Root.Solution);
      else
         Trace.Warning ("There are no changes to apply.");
      end if;
   end Del;

   ----------
   -- From --
   ----------

   procedure From (Cmd : in out Command) is
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
         Cmd.Replace_Current
           (Old_Deps     => Alire.Conditional.No_Dependencies,
            New_Deps     => Deps,
            Old_Solution => Alire.Solutions.Empty_Valid_Solution);
      else
         Trace.Warning ("No dependencies found.");
      end if;
   end From;

   ----------
   -- List --
   ----------

   procedure List (Cmd : in out Command) is
      Root_Release : constant Alire.Releases.Release := Cmd.Root.Release;
   begin
      Put_Line ("Dependencies (direct):");
      Root_Release.Dependencies.Print
        (Prefix  => "   ",
         Verbose => False,
         And_Or  => Root_Release.Dependencies.Contains_ORs,
         Sorted  => True);

      if Cmd.Solve then
         Cmd.Requires_Full_Index; -- Load possible hints
         Cmd.Root.Solution.Print (Root_Release,
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
      Cmd.Requires_Valid_Session;

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
            Cmd.Root.Solution.Print_Tree (Cmd.Root.Release);
            return;
         elsif Cmd.Graph then
            Cmd.Root.Solution.Print_Graph
              (Cmd.Root.Release, Platform.Properties);
            return;
         elsif Cmd.Versions then
            Cmd.Root.Solution.Print_Versions (Cmd.Root);
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
            if Cmd.Commit.all /= ""
              or else Alire.URI.Is_HTTP_Or_Git (Cmd.URL.all)
            then

               --  Pin to remote repo

               Add_Remote_Link (Cmd,
                                Dep => (if Num_Arguments = 1
                                        then Argument (1)
                                        else ""));

            else

               --  Pin to local folder

               if Num_Arguments = 1 then
                  Add_Softlink (Cmd,
                                Dep_Spec => Argument (1),
                                Path     => Cmd.URL.all);
               else
                  Detect_Softlink (Cmd,
                                   Cmd.URL.all);
               end if;
            end if;
         else
            Cmd.Requires_Full_Index;
            Cmd.Add;
         end if;

      elsif Cmd.Del then
         Del (Cmd);
      elsif Cmd.From then
         Cmd.Requires_Full_Index;
         From (Cmd);
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
       .Append ("When a single crate name is accompanied by an --use PATH|URL"
                & " argument, the crate is always fulfilled for any required"
                & " version by the sources found at the given target."
                & " An optional reference can be specified with --commit;"
                & " the pin will be frozen at the commit currently matching"
                & " the reference.")
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
         Output      => Cmd.Commit'Access,
         Long_Switch => "--commit=",
         Argument    => "REF",
         Help        => "Commit to retrieve from repository");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.URL'Access,
         Long_Switch => Switch_URL & "=",
         Argument    => "PATH|URL",
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
