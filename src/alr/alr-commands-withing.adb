with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

with Alire.Dependencies;
with Alire.Optional;
with Alire.Platforms.Current;
with Alire.Releases;
with Alire.Roots.Editable;
with Alire.Solutions;
with Alire.URI;
with Alire.Utils.User_Input;

with Alr.OS_Lib;

with Semantic_Versioning.Extended;

with TOML_Slicer;

package body Alr.Commands.Withing is

   Switch_URL : constant String := "--use";

   ---------
   -- Add --
   ---------

   procedure Add (Root : in out Alire.Roots.Editable.Root;
                  Args :        AAA.Strings.Vector)
   is
   begin
      for I in Args.First_Index .. Args.Last_Index loop
         Root.Add_Dependency (Alire.Dependencies.From_String (Args (I)));
      end loop;
   end Add;

   ---------
   -- Del --
   ---------

   procedure Del (Root : in out Alire.Roots.Editable.Root;
                  Args :        AAA.Strings.Vector) is
   begin
      for I in Args.First_Index .. Args.Last_Index loop
         Root.Remove_Dependency (Alire.To_Name (Args (I)));
      end loop;
   end Del;

   ----------
   -- From --
   ----------

   procedure From (Root : in out Alire.Roots.Editable.Root;
                   Args :        AAA.Strings.Vector)
   is
      use Ada.Text_IO;
      use AAA.Strings;

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
                  Root.Add_Dependency
                    (Alire.Dependencies.From_String (Line (First .. Last)));
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
      for I in Args.First_Index .. Args.Last_Index loop
         Check_File (Args (I));
      end loop;
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

      if not Root_Release.Pins.Is_Empty then
         Put_Line ("Pins (direct):");
         Root_Release.Pins.Print (Prefix => "   ");
      end if;

      if Cmd.Solve then
         Cmd.Root.Solution.Print (Root_Release,
                                  Alire.Platforms.Current.Properties,
                                  Detailed => True,
                                  Level    => Always);
      end if;
   end List;

   ------------------
   -- Add_With_Pin --
   ------------------

   procedure Add_With_Pin (Cmd  : in out Command;
                           Root : in out Alire.Roots.Editable.Root;
                           Args :        AAA.Strings.Vector)
   is
      Crate : constant Alire.Optional.Crate_Name :=
                (if Args.Count = 1
                 then Alire.Optional.Crate_Names.Unit
                   (Alire.Dependencies.From_String (Args (1)).Crate)
                 else Alire.Optional.Crate_Names.Empty);
   begin

      --  First, add the dependency if given

      if Args.Count = 1 then
         declare
            use type Semantic_Versioning.Extended.Version_Set;
            Dep : constant Alire.Dependencies.Dependency :=
                    Alire.Dependencies.From_String (Args (1));
         begin
            if Dep.Versions /= Semantic_Versioning.Extended.Any and then
              not Cmd.Root.Solution.Depends_On (Dep.Crate)
            then
               Root.Add_Dependency (Dep);
            end if;
         end;
      end if;

      --  Now, add the pin to the path/remote

      if Cmd.Commit.all /= "" or else Cmd.Branch.all /= ""
        or else Alire.URI.URI_Kind (Cmd.URL.all) in Alire.URI.Git_URIs
      then

         --  Pin to remote repo, with optional dependency first

         Root.Add_Remote_Pin
           (Crate  => Crate,
            Origin => Cmd.URL.all,
            Ref    => Cmd.Commit.all,
            Branch => Cmd.Branch.all,
            Subdir => Cmd.Subdir.all);

      else

         --  Pin to local folder, with a warning if it doesn't look like a path
         --  and a subsequent confirmation prompt if it doesn't exist.

         declare
            use Alire.URI;
            Local : constant Boolean := URI_Kind (Cmd.URL.all) in Local_URIs;
            Path  : constant String := (if Local then Local_Path (Cmd.URL.all)
                                        else Cmd.URL.all);
         begin
            if not Local then
               Alire.Put_Warning
                 ("Assuming '" & Cmd.URL.all & "' is a directory because no "
                  & "branch or commit was specified.");
            end if;

            if Cmd.Subdir.all /= "" then
               Reportaise_Wrong_Arguments
                 ("Pins to local directories do not accept the "
                  & TTY.Terminal ("--subdir") & " switch");
            end if;

            if not Alire.Utils.User_Input.Approve_Dir (Path) then
               Trace.Info ("Abandoned by user.");
               return;
            end if;

            Root.Add_Path_Pin (Crate => Crate, Path  => Path);
         end;

      end if;
   end Add_With_Pin;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Cmd  : in out Command;
                      Args :        AAA.Strings.Vector)
   is
      Flags : Natural := 0;

      -----------
      -- Check --
      -----------

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
      Cmd.Requires_Workspace;

      if Cmd.URL.all /= "" then
         Flags := Flags + 1;
      end if;

      Check (Cmd.Del);
      Check (Cmd.From);
      Check (Cmd.Graph);
      Check (Cmd.Solve);
      Check (Cmd.Tree);
      Check (Cmd.Versions);

      if Cmd.Commit.all /= "" and then Cmd.Branch.all /= "" then
         Reportaise_Wrong_Arguments
           ("Cannot specify both a branch and a commit simultaneously");
      end if;

      --  No parameters: give requested info and return. There is still the
      --  possibility of a `with --use` that is processed later.

      if Args.Count = 0 then
         if Flags = 0 or else Cmd.Solve then
            List (Cmd);
            return;
         elsif Cmd.Tree then
            Cmd.Root.Solution.Print_Tree (Cmd.Root.Release);
            return;
         elsif Cmd.Graph then
            Cmd.Root.Solution.Print_Graph
              (Cmd.Root.Release, Alire.Platforms.Current.Properties);
            return;
         elsif Cmd.Versions then
            Cmd.Root.Solution.Print_Versions (Cmd.Root);
            return;
         end if;
      end if;

      if Args.Count < 1 then
         if Cmd.Del then
            Reportaise_Wrong_Arguments ("At least one dependency required");
         elsif Cmd.From then
            Reportaise_Wrong_Arguments
              ("At least one GPR file to process required");
         end if;
      end if;

      --  At this point, we are modifying the manifest to incorporate changes,
      --  so we create a temporary copy of the root for these changes

      declare
         New_Root : Alire.Roots.Editable.Root :=
                      Alire.Roots.Editable.New_Root (Cmd.Root);
      begin

         if not (Cmd.Del or else Cmd.From) then

            --  Must be Add, but it could be regular or softlink

            if Cmd.URL.all /= "" then
               Cmd.Add_With_Pin (New_Root, Args);
            else
               Cmd.Auto_Update_Index;
               Add (New_Root, Args);
            end if;

         elsif Cmd.Del then
            Del (New_Root, Args);
         elsif Cmd.From then
            Cmd.Auto_Update_Index;
            From (New_Root, Args);
         else
            raise Program_Error with "List should have already happened";
         end if;

         --  Apply changes

         New_Root.Confirm_And_Commit;

      end;
   exception
      when E : TOML_Slicer.Slicing_Error =>
         Alire.Log_Exception (E);
         Reportaise_Command_Failed
           ("alr was unable to apply your request; "
            & "please edit the manifest manually.");
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (Cmd : Command)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
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
                & " the reference. Alternatively, a branch to track can be"
                & " specified with --branch. Use `alr update` to refresh the"
                & " tracking pin contents.")
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
       .Append (Crate_Version_Sets));

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Command;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
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
         Output      => Cmd.Branch'Access,
         Long_Switch => "--branch=",
         Argument    => "NAME",
         Help        => "Branch to track in repository");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Commit'Access,
         Long_Switch => "--commit=",
         Argument    => "REF",
         Help        => "Commit to retrieve from repository");

      Define_Switch
        (Config      => Config,
         Output      => Cmd.Subdir'Access,
         Long_Switch => "--subdir=",
         Argument    => "REL_PATH",
         Help        => "Relative path to crate inside repository");

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
